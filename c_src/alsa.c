#define _GNU_SOURCE
#include <erl_nif.h>
#include <assert.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <alsa/asoundlib.h>


static char *detect_pcm();


typedef struct
{
    uint32_t num,den;
    uint32_t counter;
    // uint32_t size;
    uint32_t sample_rate;
    uint32_t channels;
    uint32_t frame_size;
    uint8_t buffer[4096];
    char *pcm_name;
    snd_pcm_t *handle;
    // int selectedDevice;
    int running;
    ErlNifPid owner_pid;
    ErlNifTid tid;
    ERL_NIF_TERM port;
    ErlNifEnv* env;
    int thread_started;
} AudioCapture;


ErlNifResourceType* alsa_resource;


static void
alsa_destructor(ErlNifEnv* env, void* obj)
{
  AudioCapture *capture = (AudioCapture *)obj;
  fprintf(stderr, "Hm, dealloc: %s\r\n", capture->pcm_name);
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  alsa_resource = enif_open_resource_type(env, NULL, "alsa_resource", alsa_destructor, ERL_NIF_RT_CREATE, NULL);
  return 0;
}



void *capture_thread(void *data) {
  AudioCapture *capture = (AudioCapture *)data;
  enif_keep_resource(capture);
  
  capture->pcm_name = detect_pcm();
  snd_pcm_open(&capture->handle, capture->pcm_name, SND_PCM_STREAM_CAPTURE, 0);
  if(!capture->handle) {
    fprintf(stderr, "No PCM!!\r\n");
    exit(1);
  }
  snd_pcm_hw_params_t *hw_params;
  snd_pcm_hw_params_malloc(&hw_params);
  if(!hw_params) {
    fprintf(stderr, "Damn!! No hw_params\r\n");
    exit(1);
  }
  
  snd_pcm_hw_params_any(capture->handle, hw_params);
  snd_pcm_hw_params_set_access(capture->handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED);
  snd_pcm_hw_params_set_format(capture->handle, hw_params, SND_PCM_FORMAT_S16_LE);
  snd_pcm_hw_params_set_rate_near(capture->handle, hw_params, &capture->sample_rate, 0);
  snd_pcm_hw_params_set_channels(capture->handle, hw_params, capture->channels);
  
  int buffer_size = 16384;
	int period_time = 32000;
  int period_size = 1024;
  // snd_pcm_hw_params_set_period_time_near(capture->handle, hw_params, &period_time, 0);
  // snd_pcm_hw_params_set_period_size_near(capture->handle, hw_params, &period_size, 0);
  //  snd_pcm_hw_params_set_buffer_size_near(capture->handle, hw_params, &buffer_size);
  //  
	
  fprintf(stderr, "Setting params: %p, %p\r\n", capture->handle, hw_params);
  
  snd_pcm_hw_params(capture->handle, hw_params);
	snd_pcm_hw_params_free(hw_params);
  snd_pcm_prepare(capture->handle);
  
  snd_output_t *log;
  snd_output_stdio_attach(&log, stderr, 0);
  snd_pcm_hw_params_dump(hw_params, log);
  
  fprintf(stderr, "Started capture\r\n");
  char *buffer = (char *)malloc(8192);
  
  char *ptr = buffer;
  int size = 0;
  
  while(capture->thread_started) {
    int r = snd_pcm_readi(capture->handle, buffer + size, 1024);
    size += r*2*capture->channels;
    if(size < capture->frame_size) {
      continue;
    }
    
    ErlNifEnv* env = enif_alloc_env();
    ErlNifBinary frame;
    
    enif_alloc_binary(capture->frame_size, &frame);
    memmove(frame.data, buffer, frame.size);
    size -= frame.size;
    memmove(buffer, buffer + frame.size, size);
    
    uint32_t dts = capture->counter*1024*1000 / capture->sample_rate;
    
    // fprintf(stderr, "A: %d -> %d\r\n", capture->counter, dts);
    
    enif_send(NULL, &capture->owner_pid, env, 
      enif_make_tuple4(env,
        enif_make_atom(env, "alsa"),
        enif_make_resource(env, capture),
        enif_make_uint(env, dts),
        enif_make_binary(env, &frame)
      )
    );
    
    enif_release_binary(&frame);
    
    enif_free_env(env);
    capture->counter++;
    
  }
  enif_release_resource(capture);
  snd_pcm_close(capture->handle);
  return 0;
}

char *detect_pcm() {
  void **hints, **n;
  char *name, *desc, *io;
  
  snd_device_name_hint(-1, "pcm", &hints);
  n = hints;
  
  char *detected_name = NULL;
  
  while(*n && !detected_name) {
		io = snd_device_name_get_hint(*n, "IOID");
		if(!io || strcmp(io, "Input")) {
      if(io) free(io);
      n++;
      continue;
		}
    name = snd_device_name_get_hint(*n, "NAME");
		desc = snd_device_name_get_hint(*n, "DESC");
		if(strstr(desc, "USB") && strstr(desc, "Default Audio")) {
      detected_name = (char *)malloc(strlen(name) + 1);
      strcpy(detected_name, name);
		}
    free(name);
    free(desc);
    n++;
  }
  
  snd_device_name_free_hint(hints);
  return detected_name;
}

static ERL_NIF_TERM
alsa_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int selectedDevice, sample_rate, channels;
  
  enif_get_int(env, argv[0], &sample_rate);
  enif_get_int(env, argv[1], &channels);

  // selectedDevice = dump_audio_info(selectedDevice);

  AudioCapture *capture = (AudioCapture *)enif_alloc_resource(alsa_resource, sizeof(AudioCapture));
  bzero(capture, sizeof(AudioCapture));

  capture->sample_rate = sample_rate;
  capture->channels = channels;
  capture->frame_size = capture->channels*2*1024;

  
  enif_self(env, &capture->owner_pid);
  capture->env = enif_alloc_env();
  
  fprintf(stderr, "Starting alsa %d\r\n", capture->sample_rate);
  
  capture->thread_started = 1;
  enif_thread_create("alsa_thread", &capture->tid, capture_thread, capture, NULL);
  
  ERL_NIF_TERM port = enif_make_resource(env, capture);
  capture->port = enif_make_copy(capture->env, port);

  return port;
}


static ErlNifFunc alsa_funcs[] =
{
    {"real_start", 2, alsa_init}
};

ERL_NIF_INIT(alsa, alsa_funcs, load, 0, 0, 0)
