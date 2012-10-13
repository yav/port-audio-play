#ifndef PLAY_H
#define PLAY_H

#include <portaudio.h>


#define SAMPLE short

typedef struct sample {
  unsigned long frame_num;    // Number of frames stored in the bufffer.
  unsigned long next_frame;   // The next frame from sample to play.
  struct sample *next_sample; // Place this, once we are finished.
  SAMPLE data[];
} sample;

typedef struct {

  // Read only.  The number of channels for the stream.
  unsigned long channels;

  sample* cur_sample;         // Current sample from which to read.

  // Communication with the callback.
  volatile sample * next_sample;        // Start playing this ASAP.
                                        // Client should only write here if NULL

  PaStream *stream;

} stream_state;


PaError playInit
  ( stream_state * s
  , unsigned long chan_num
  , double        sample_rate
  , unsigned long framer_per_buffer   // 0 for auto.
  );


void playFrames(stream_state *s, unsigned long frames, SAMPLE *data);
void playCleanup (stream_state *s);
#endif


