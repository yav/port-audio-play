#include <stdlib.h>
#include <string.h>
#include <play.h>

static inline
unsigned long to_bytes(stream_state *s, unsigned long x) {
  return x * s->channels * sizeof(SAMPLE);
}

static
int callback
  ( const void *input,
    void *output,
    unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData
  )
{
  stream_state *state = userData;
  sample *cur         = state->cur_sample;
  unsigned long done  = 0;


  while (done < frameCount) {
    unsigned long have = cur->frame_num - state->next_frame;
    unsigned long todo = frameCount - done;

    if (todo > have) todo = have;

    memcpy( output + to_bytes(state,done)
          , &cur->data[state->next_frame * state->channels]
          , to_bytes(state, todo)
          );
    state->next_frame += todo;
    done              += todo;

    if (state->next_frame == cur->frame_num) {
      cur = state->cur_sample = state->next_sample;
      state->next_frame = 0;
      if (! state->loop) state->next_sample = NULL;
      if (cur == NULL) return paComplete;
    }
  }

  return paContinue;
}

PaError playInit
  ( stream_state * s
  , unsigned long chan_num
  , double        sample_rate
  , unsigned long frames_per_buffer   // 0 for auto.
  ) {

  PaError err;

  s->cur_sample = NULL;
  s->loop       = 0;
  s->channels   = chan_num;

  err = Pa_Initialize();
  if (err != paNoError) return err;

  return Pa_OpenDefaultStream( &s->stream
                             , 0, chan_num, paInt16, sample_rate
                             , frames_per_buffer
                             , callback
                             , s
                             );
}


void playCleanup (stream_state *s) {
  (void) Pa_CloseStream(s->stream);
  (void) Pa_Terminate();
}

PaError playStart (stream_state *s) {
  if (s->next_sample == NULL) return paBadStreamPtr;  // hm, reusing errs...
  s->cur_sample = s->next_sample;
  s->next_frame = 0;
  if (!s->loop) s->next_sample = NULL;
  return Pa_StartStream(s->stream);
}
PaError playStop  (stream_state *s) { return Pa_StopStream(s->stream); }
PaError playAbort (stream_state *s) { return Pa_AbortStream(s->stream); }

void playLooping (stream_state *s, int yes) { s->loop = yes; }
void playNext (stream_state *s, sample *next) { s->next_sample = next; }

sample *mallocSample(stream_state *s, unsigned long frames) {
  sample *m = (sample*) malloc(sizeof(sample) +
                               sizeof(SAMPLE) * frames * s->channels);
  if (m == NULL) return m;
  m->frame_num = 0;
  m->max_frames = frames;
  return m;
}



