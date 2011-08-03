#include <erl_driver.h>
#include <assert.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <portaudio.h>



// unsigned char raw_buffer[SAMPLE_SIZE*BUFFER_CAPACITY];
// unsigned char buffer[8192];
// unsigned long max_bytes;

typedef struct
{
    uint32_t num,den;
    uint32_t counter;
    uint32_t size;
    uint32_t sample_rate;
    uint32_t channels;
    uint32_t frame_size;
    uint8_t buffer[8192];
    int selectedDevice;
    int running;
    ErlDrvPort port;
    ErlDrvTermData owner_pid;
} AudioCapture;



static int audioCaptured( const void *inputBuffer, void *outputBuffer,
                           unsigned long framesPerBuffer,
                           const PaStreamCallbackTimeInfo* timeInfo,
                           PaStreamCallbackFlags statusFlags,
                           void *userData )
{
    AudioCapture *capture = (AudioCapture *)userData;

    if(capture->size + framesPerBuffer*2*capture->channels > sizeof(capture->buffer)) {
      fprintf(stderr, "audio buffer overflow\r\n");
      capture->size = 0;
      ErlDrvTermData reply[] = {
        ERL_DRV_ATOM, driver_mk_atom("audiocapture_error"),
        ERL_DRV_PORT, driver_mk_port(capture->port),
        ERL_DRV_ATOM, driver_mk_atom("buffer_overflow"),
        ERL_DRV_TUPLE, 3
      };
      driver_output_term(capture->port, reply, sizeof(reply) / sizeof(reply[0]));
      
      return -1;
    }
    memmove(capture->buffer + capture->size, inputBuffer, framesPerBuffer*2*capture->channels);
    capture->size += framesPerBuffer*2*capture->channels;
    
    if(capture->size > capture->frame_size) {
      ErlDrvBinary* frame = driver_alloc_binary(capture->frame_size);
      memmove(frame->orig_bytes, capture->buffer, frame->orig_size);
      uint32_t dts = capture->counter*1024*1000 / capture->sample_rate;
      
      ErlDrvTermData reply[] = {
        ERL_DRV_ATOM, driver_mk_atom("audiocapture"),
        ERL_DRV_PORT, driver_mk_port(capture->port),
        ERL_DRV_UINT, dts,
        ERL_DRV_BINARY, (ErlDrvTermData)frame, (ErlDrvTermData)capture->frame_size, 0,
        ERL_DRV_TUPLE, 4
      };
      driver_output_term(capture->port, reply, sizeof(reply) / sizeof(reply[0]));
      
      capture->size -= capture->frame_size;
      capture->counter++;
      memmove(capture->buffer, capture->buffer + capture->frame_size, capture->size);
    }
    
    return 0;
}





static ErlDrvData audiocapture_drv_start(ErlDrvPort port, char *buff)
{
    AudioCapture* d = (AudioCapture *)driver_alloc(sizeof(AudioCapture));
    bzero(d, sizeof(AudioCapture));
    d->port = port;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    d->owner_pid = driver_caller(port);
    return (ErlDrvData)d;
}


static void audiocapture_drv_stop(ErlDrvData handle)
{
  Pa_Terminate();
  // FIXME: stop capture
  driver_free((char*)handle);
}

 
int dump_audio_info(int selectedDevice)
{
    PaError err;
    err = Pa_Initialize();
    if( err != paNoError ) {
        fprintf(stderr, "Portaudio error: %s\r\n", Pa_GetErrorText(err));
        exit(1);
    }
    
    
    int numDevices = Pa_GetDeviceCount();
    if(numDevices < 0) {
        fprintf(stderr, "getDevices error: %s\r\n", Pa_GetErrorText(numDevices));
        exit(1);
    }
    int i;
    
    // fprintf(stderr, "Default device: %d, selected: %d\r\n", Pa_GetDefaultInputDevice(), selectedDevice);
    const PaDeviceInfo *deviceInfo;
    for(i = 0; i < numDevices; i++) {
        deviceInfo = Pa_GetDeviceInfo(i);
        if(selectedDevice == -1 && strstr(deviceInfo->name, "USB Audio")) {
          selectedDevice = i;
        }
        if(deviceInfo->maxInputChannels > 0) {
            fprintf(stderr, "Device(%d): %s (%d, %d) %s,%s\r\n", i, deviceInfo->name, deviceInfo->maxInputChannels, deviceInfo->hostApi, 
                i == Pa_GetDefaultInputDevice() ? "default" : "-",
                i == selectedDevice ? "selected" : "-");
        }
    }
    
    return selectedDevice;
}

static int audiocapture_drv_command(ErlDrvData handle, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen)
{
  AudioCapture *capture = (AudioCapture *)handle;
  int selectedDevice, sample_rate, channels;
  
  selectedDevice = buf[0];
  channels = buf[1];
  sample_rate = htons(*(uint16_t *)(buf + 2));

  PaStream *stream;
  PaError err;

  selectedDevice = dump_audio_info(selectedDevice);

  const PaDeviceInfo *deviceInfo;

  deviceInfo = Pa_GetDeviceInfo(selectedDevice);


  PaStreamParameters inputParameters;
  bzero( &inputParameters, sizeof( inputParameters ) );

  inputParameters.channelCount = channels;
  inputParameters.device = selectedDevice;
  inputParameters.sampleFormat = paInt16; //paFloat32;
  inputParameters.suggestedLatency = deviceInfo->defaultLowInputLatency;
  inputParameters.hostApiSpecificStreamInfo = NULL;

  err = Pa_IsFormatSupported(&inputParameters, NULL, sample_rate);
  if(err != paNoError) {
      fprintf(stderr, "Invalid configuration: %s\r\n", Pa_GetErrorText(err));
      driver_failure_atom(capture->port, "invalid_parameters");
      return 0;
  }


  capture->selectedDevice = selectedDevice;
  capture->sample_rate = sample_rate;
  capture->channels = channels;
  capture->frame_size = capture->channels*2*1024;

  err = Pa_OpenStream( &stream,
                       &inputParameters,
                       NULL,
                       capture->sample_rate,
                       paFramesPerBufferUnspecified,        /* frames per buffer, i.e. the number
                                         of sample frames that PortAudio will
                                         request from the callback. Many apps
                                         may want to use
                                         paFramesPerBufferUnspecified, which
                                         tells PortAudio to pick the best,
                                         possibly changing, buffer size.*/
                      paNoFlag,
                      audioCaptured, /* this is your callback function */
                      capture ); /*This is a pointer that will be passed to
                                                     your callback*/
  if( err != paNoError ) {
    driver_failure_atom(capture->port, Pa_GetErrorText(err));
    return 0;
  }

  err = Pa_StartStream(stream);
  if( err != paNoError ) {
    driver_failure_atom(capture->port, Pa_GetErrorText(err));
    return 0;
  }


  // Pa_Terminate();
  
  memcpy(*rbuf, "ok", 2);
  return 2;
}



ErlDrvEntry audiocapture_driver_entry = {
    NULL,			/* F_PTR init, N/A */
    audiocapture_drv_start,		/* L_PTR start, called when port is opened */
    audiocapture_drv_stop,		/* F_PTR stop, called when port is closed */
    NULL,	                /* F_PTR output, called when erlang has sent */
    NULL,		            /* F_PTR ready_input, called when input descriptor ready */
    NULL,	              /* F_PTR ready_output, called when output descriptor ready */
    "audiocapture_drv",		/* char *driver_name, the argument to open_port */
    NULL,			/* F_PTR finish, called when unloaded */
    NULL,     /* void *handle */
    audiocapture_drv_command,			/* F_PTR control, port_command callback */
    NULL,		    	/* F_PTR timeout, reserved */
    NULL,	                     /* F_PTR outputv, reserved */
    NULL,                      /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,     /* ERL_DRV_FLAGs */
    NULL,     /* void *handle2 */
    NULL,     /* process_exit */
    NULL      /* stop_select */
};
DRIVER_INIT(audiocapture_drv) /* must match name in driver_entry */
{
    return &audiocapture_driver_entry;
}

