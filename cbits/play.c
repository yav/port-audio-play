#include <stdlib.h>
#include <string.h>
#include <play.h>
#include <stdio.h>

static inline
unsigned long to_bytes(stream_state *s, unsigned long x) {
  return x * s->channels * sizeof(SAMPLE);
}


// S1 is modified to be the mix of S1 and S2.
// If S2 was longer, then the reminder is enqueued after S1.
// Assumes that S2 is a sinlge (i.e., not a list) sample.
static
void mixSamples (stream_state *s, sample *s1, sample *s2) {
  unsigned long channels = s->channels;
  unsigned long p1,p2,c;

  for ( p1 = s1->next_frame, p2 = s2->next_frame
      ; p1 < s1->frame_num && p2 < s2->frame_num
      ; ++p1, ++p2
      )
    for (c = 0; c < channels; ++ c) {
      unsigned long ix = channels * p1 + c;
      s1->data[ix] = s1->data[ix] / 2 + s2->data[channels * p2 + c] / 2;
    }

  s2->next_frame = p2;

  if (s2->next_frame == s2->frame_num) {
    free(s2);
    return;
  }
  if (s1->next_sample == NULL) {
    s1->next_sample = s2;
    return;
  }
  mixSamples(s, s1->next_sample, s2);
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
  unsigned long done  = 0;

  while (done < frameCount) {
    unsigned long have;
    unsigned long todo;
    sample *cur;

    // First check if we need to mix-in a new sample.
    if (state->next_sample != NULL) {

      if (state->cur_sample == NULL)
        state->cur_sample = (sample*)state->next_sample;
      else
        mixSamples(state, state->cur_sample, (sample*)state->next_sample);

      state->next_sample = NULL;
    } else
      if (state->cur_sample == NULL) return paComplete;

    cur  = state->cur_sample;   // Not NULL
    have = cur->frame_num - cur->next_frame;
    todo = frameCount - done;

    if (todo > have) todo = have;

    memcpy( output + to_bytes(state,done)
          , &cur->data[cur->next_frame * state->channels]
          , to_bytes(state, todo)
          );
    cur->next_frame += todo;
    done            += todo;

    if (cur->next_frame == cur->frame_num) {
      // Current sample run out, keep going with next part.
      state->cur_sample = cur->next_sample;
      free(cur);
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

  s->cur_sample   = NULL;
  s->next_sample  = NULL;
  s->channels     = chan_num;

  err = Pa_Initialize();
  if (err != paNoError) return err;

  return Pa_OpenDefaultStream( &s->stream
                             , 0, chan_num, paInt16, sample_rate
                             , frames_per_buffer
                             , callback
                             , s
                             );
}


// One thread at a time.
// XXX: no error reporting
// XXX: we could do the allocation separately to minimize single threaded time
void playFrames(stream_state *s, unsigned long frames, SAMPLE *data) {
  unsigned long bytes = to_bytes(s,frames);
  sample *m = (sample*) malloc(sizeof(sample) + bytes);
  if (m == NULL) return;  // Not enough memory.

  m->frame_num     = frames;
  m->next_frame   = 0;
  m->next_sample  = NULL;
  memcpy(m->data, data, bytes);

  while(s->next_sample != NULL);  // wait for stream to pick up previous.
  s->next_sample = m;

  if (! Pa_IsStreamActive(s->stream) )
    Pa_StartStream(s->stream);
}




static
void freeSample(sample *s) {
  while (s != NULL) {
    sample *p = s;
    s = s->next_sample;
    free(p);
  }
}

void playCleanup (stream_state *s) {
  (void) Pa_CloseStream(s->stream);
  (void) Pa_Terminate();
  freeSample(s->cur_sample);
  freeSample((sample*)s->next_sample);
}


