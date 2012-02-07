#ifndef PLAY_H
#define PLAY_H

#include <portaudio.h>


#define SAMPLE short

typedef struct {
  unsigned long max_frames;   // Number of frames that can fit here.
  unsigned long frame_num;    // Number of frames stored in the bufffer.
  SAMPLE data[];
} sample;

typedef struct {

  // Read only.  The number of channels for the stream.
  unsigned long channels;

  // Internal state of the callback.
  // Invariant: cur_sample not NULL while playing.
  sample* cur_sample;         // Current sample from which to read.
  unsigned long next_frame;   // The next frame from sample to play.

  // Communication with the callback.
  sample * next_sample;        // Continue playing this.

  unsigned long loop:1;       // Indicates if we want looping.
  // If looping is enabled, when the callbakc starts using the sample,
  // it leaves the 'next_sample' unmodified, so that it will keep plaing the
  // same sample over and over again.  If looping is not enabled, then
  // the driver will replace 'next_sample' with NULL as soon as it gets
  // started with it.

  PaStream *stream;

} stream_state;


PaError playInit
  ( stream_state * s
  , unsigned long chan_num
  , double        sample_rate
  , unsigned long framer_per_buffer   // 0 for auto.
  );


void playCleanup (stream_state *s);

PaError playStart (stream_state *s);
PaError playStop  (stream_state *s);
PaError playAbort (stream_state *s);

void playLooping(stream_state *s, int yes);
void playNext(stream_state *s, sample *next);

sample *mallocSample(stream_state *s, unsigned long frame_num);
#endif


