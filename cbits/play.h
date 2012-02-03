#ifndef PLAY_H
#define PLAY_H

#include <portaudio.h>

typedef struct {
  unsigned long frame_num;    // Number of frames stored in the bufffer.
  unsigned char data[];
} chunk;

typedef struct {

  // Read only.  The number of channels * number of bytes per sample.
  unsigned long frame_size;

  // Internal state of the callback.
  chunk* cur_chunk;           // Current chunk from which to read.
  unsigned long next_frame;   // The next frame from chunk to play.

  // Communication with the callback.
  chunk* next_chunk;          // Continue playing this.

  unsigned long loop:1;       // Indicates if we want looping.
  // If looping is enabled, when the callbakc starts using the chunk,
  // it leaves the 'next_chunk' unmodified, so that it will keep plaing the
  // same chunk over and over again.  If looping is not enabled, then
  // the driver will replace 'next_chunk' with NULL as soon as it gets
  // started with it.

  PaStream *stream;

} stream_state;


PaError playInit
  ( stream_state * s
  , unsigned long chan_num
  , unsigned long sample_format
  , double        sample_rate
  , unsigned long framer_per_buffer   // 0 for auto.
  );


void playCleanup (stream_state *s);

PaError playStart (stream_state *s);
PaError playStop  (stream_state *s);
PaError playAbort (stream_state *s);

void playLooping(stream_state *s, int yes);
void playNext(stream_state *s, chunk *next);

#endif


