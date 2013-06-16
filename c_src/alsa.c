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
    int pcm_id;
    snd_pcm_t *handle;
    int selected_device;
    int running;
    ErlNifPid owner_pid;
    ErlNifTid tid;
    ERL_NIF_TERM port;
    ErlNifEnv* env;
    int thread_started;
    
    ErlNifUInt64 last_dts;
} AudioCapture;


ErlNifResourceType* alsa_resource;

static void detect_pcm(AudioCapture *capture);

static void
alsa_destructor(ErlNifEnv* env, void* obj)
{
  AudioCapture *capture = (AudioCapture *)obj;
  if(capture->thread_started) {
    capture->thread_started = 0;
    enif_thread_join(capture->tid, NULL);
  }
  fprintf(stderr, "Hm, dealloc: %s\r\n", capture->pcm_name);
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  alsa_resource = enif_open_resource_type(env, NULL, "alsa_resource", alsa_destructor, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
  return 0;
}

static void set_volume(AudioCapture *capture, int level) {
  snd_mixer_t *mixer=NULL;
  
  int err;
  err=snd_mixer_open(&mixer,0);
  if(err < 0) {
    fprintf(stderr, "Could not open alsa mixer: %s\r\n",snd_strerror(err));
    exit(1);
  }
  
  char hw_id[100];
  snprintf(hw_id, sizeof(hw_id), "hw:%d", capture->pcm_id);

  if ((err = snd_mixer_attach (mixer, hw_id)) < 0){
		fprintf(stderr, "Could not attach mixer to card(%s): %s\r\n", hw_id, snd_strerror(err));
		snd_mixer_close(mixer);
    exit(1);
	}
	if ((err = snd_mixer_selem_register (mixer, NULL, NULL)) < 0){
		fprintf(stderr, "snd_mixer_selem_register: %s\r\n",snd_strerror(err));
		snd_mixer_close(mixer);
		exit(1);
	}
	if ((err = snd_mixer_load (mixer)) < 0){
		fprintf(stderr, "snd_mixer_load: %s\r\n",snd_strerror(err));
		snd_mixer_close(mixer);
		exit(1);
	}
	
	snd_mixer_elem_t *elem;
	elem = snd_mixer_first_elem(mixer);
	
	while(elem) {
    const char *elemname = snd_mixer_selem_get_name(elem);
    if(strcmp(elemname, "Capture") && strcmp(elemname, "Mic")) {
      fprintf(stderr, "Skip setting volume for %s\r\n", elemname);
      elem=snd_mixer_elem_next(elem);
      continue;
    }
  	if (snd_mixer_selem_has_capture_volume(elem)){
      fprintf(stderr, "Set volume for %s\r\n", elemname);
    	long sndMixerPMin;
    	long sndMixerPMax;
      long newvol;
    	snd_mixer_selem_get_playback_volume_range(elem, &sndMixerPMin, &sndMixerPMax);
    	newvol=(((sndMixerPMax-sndMixerPMin)*level)/100)+sndMixerPMin;
      snd_mixer_selem_set_capture_volume_all(elem,newvol);
      elem=snd_mixer_elem_next(elem);
  	} else {
      fprintf(stderr, "Can't set capture volume\r\n");
      exit(1);
  	}
	}
}


void *capture_thread(void *data) {
  AudioCapture *capture = (AudioCapture *)data;
  // enif_keep_resource(capture);
  
  detect_pcm(capture);
  
  set_volume(capture, 100);
  
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
  
  // int buffer_size = 16384;
  // int period_time = 32000;
  // int period_size = 1024;
  // snd_pcm_hw_params_set_period_time_near(capture->handle, hw_params, &period_time, 0);
  // snd_pcm_hw_params_set_period_size_near(capture->handle, hw_params, &period_size, 0);
  //  snd_pcm_hw_params_set_buffer_size_near(capture->handle, hw_params, &buffer_size);
  //  
	
  capture->last_dts = 0;
  
  fprintf(stderr, "Setting params: %p, %p\r\n", capture->handle, hw_params);
  
  snd_pcm_hw_params(capture->handle, hw_params);
	snd_pcm_hw_params_free(hw_params);
  snd_pcm_prepare(capture->handle);
  
  snd_output_t *log;
  snd_output_stdio_attach(&log, stderr, 0);
  snd_pcm_hw_params_dump(hw_params, log);
  
  fprintf(stderr, "Started capture\r\n");
  char *buffer = (char *)malloc(8192);
  
  // char *ptr = buffer;
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
    
    ErlNifUInt64 dts = (uint64_t)capture->counter*1024ULL*1000 / capture->sample_rate;
    
    // fprintf(stderr, "A: %d -> %d\r\n", capture->counter, dts);
    
    if(capture->last_dts > dts) {
      fprintf(stderr, "Achtung! ALSA audio jump: %u, %u, %u\r\n", (unsigned)capture->counter, (unsigned)capture->last_dts, (unsigned)dts);
    }
    capture->last_dts = dts;
    enif_send(NULL, &capture->owner_pid, env, 
      enif_make_tuple4(env,
        enif_make_atom(env, "alsa"),
        enif_make_resource(env, capture),
        enif_make_uint64(env, dts),
        enif_make_binary(env, &frame)
      )
    );
    
    enif_release_binary(&frame);
    
    enif_free_env(env);
    capture->counter++;
  }
  fprintf(stderr, "Capture thread stopping\r\n");
  // enif_release_resource(capture);
  snd_pcm_close(capture->handle);
  return 0;
}

void detect_pcm(AudioCapture *capture) {
  void **hints, **n;
  char *name, *desc, *io;
  
  snd_device_name_hint(-1, "pcm", &hints);
  n = hints;
  
  char *detected_name = NULL;
    
  int pcm_id = 0;
  fprintf(stderr, "Selecting (%d)\r\n", capture->selected_device);
  
  while(*n && !detected_name) {
		io = snd_device_name_get_hint(*n, "IOID");
    if(!io || strcmp(io, "Input")) {
      if(io) free(io);
      n++;
      pcm_id++;
      continue;
    }
    name = snd_device_name_get_hint(*n, "NAME");
    desc = snd_device_name_get_hint(*n, "DESC");
    if((capture->selected_device < 0 && strstr(desc, "USB") && strstr(desc, "Default Audio")) || capture->selected_device == pcm_id) {
      detected_name = (char *)malloc(strlen(name) + 1);
      strcpy(detected_name, name);
    }
    free(name);
    free(desc);
    n++;
  }
  
  snd_device_name_free_hint(hints);
  // This is so for USB
  //capture->pcm_id = 1;
  
  fprintf(stderr, "Selected (%d)\r\n", pcm_id);
  capture->pcm_name = detected_name;
}

static ERL_NIF_TERM
alsa_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int sample_rate, channels;
  int selected_device = -1;
  
  enif_get_int(env, argv[0], &sample_rate);
  enif_get_int(env, argv[1], &channels);
  if(argc > 2) {
    enif_get_int(env, argv[2], &selected_device);
  }

  // selectedDevice = dump_audio_info(selectedDevice);

  AudioCapture *capture = (AudioCapture *)enif_alloc_resource(alsa_resource, sizeof(AudioCapture));
  bzero(capture, sizeof(AudioCapture));

  capture->sample_rate = sample_rate;
  capture->channels = channels;
  capture->frame_size = capture->channels*2*1024;
  capture->selected_device = selected_device;

  
  enif_self(env, &capture->owner_pid);
  capture->env = enif_alloc_env();
  
  fprintf(stderr, "Starting alsa %d\r\n", capture->sample_rate);
  
  capture->thread_started = 1;
  enif_thread_create("alsa_thread", &capture->tid, capture_thread, capture, NULL);
  
  ERL_NIF_TERM port = enif_make_resource(env, capture);
  capture->port = enif_make_copy(capture->env, port);

  enif_release_resource(capture);
  return port;
}

static ERL_NIF_TERM
alsa_stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  
  AudioCapture *capture;
  
  if(!enif_get_resource(env, argv[0], alsa_resource, (void **)&capture)) {
    return enif_make_badarg(env);
  }
  
  if(capture->thread_started) {
    capture->thread_started = 0;
    enif_thread_join(capture->tid, NULL);
  }
  
  return enif_make_atom(env, "ok");
}


static ErlNifFunc alsa_funcs[] =
{
//    {"real_start", 2, alsa_init},
    {"real_start", 3, alsa_init},
    {"stop", 1, alsa_stop}
};

ERL_NIF_INIT(alsa, alsa_funcs, load, 0, 0, 0)
