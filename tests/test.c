#include "play.h"
#include <sndfile.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>




stream_state data;

int main(int argc, char *argv[]) {
  PaError err;
  SF_INFO sfinfo;
  SNDFILE *file;
  chunk *c;

  if (argc < 2) {
    fprintf(stderr, "Usage %s FILE\n", argv[0]);
    return 1;
  }

  file = sf_open(argv[1], SFM_READ, &sfinfo);
  printf("%d frames %d samplerate %d channels\n",
            (int)sfinfo.frames, sfinfo.samplerate, sfinfo.channels);

  c = (chunk*) malloc(sizeof(chunk) + 1024 * 1024);
  c->frame_num = sf_read_short(file, (short int*)c->data,
                              (1024*1024)/sizeof(short)) / sfinfo.channels;

  sf_close(file);

  err = playInit(&data, 1, paInt16, 11025, 0);
  if( err != paNoError ) goto error;

  playNext(&data, c);

  err = playStart(&data);
  if ( err != paNoError ) goto error;

  Pa_Sleep(5 * 1000);

  err = playStop(&data);
  if ( err != paNoError ) goto error;

  playCleanup(&data);
  return 0;

error:

  fprintf(stderr, "PortAudio error: %s\n", Pa_GetErrorText( err ) );
  return 1;
}

