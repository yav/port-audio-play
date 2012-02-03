#include <stdlib.h>
#include <string.h>
#include <play.h>

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
  stream_state *state      = userData;
  unsigned long frame_size = state->frame_size;

  while (frameCount > 0) {
    unsigned long have;
    unsigned long todo;

    if (state->cur_chunk == NULL) {
      state->cur_chunk  = state->next_chunk;
      state->next_frame = 0;
      if (! state->loop) state->next_chunk = NULL;
    }

    if (state->cur_chunk == NULL) return paComplete;

    have = state->cur_chunk->frame_num - state->next_frame;
    todo = frameCount;
    if (todo > have) todo = have;
    memcpy( output
          , &state->cur_chunk->data[state->next_frame * frame_size]
          , frame_size * todo
          );
    state->next_frame += todo;
    frameCount        -= todo;

    if (state->next_frame == state->cur_chunk->frame_num) {
      state->cur_chunk = NULL;
    }
  }

  return paContinue;
}

PaError playInit
  ( stream_state * s
  , unsigned long chan_num
  , unsigned long sample_format
  , double        sample_rate
  , unsigned long frames_per_buffer   // 0 for auto.
  ) {

  PaError err;

  s->cur_chunk  = NULL;
  s->loop       = 0;
  s->frame_size = chan_num;

  switch (sample_format) {
    case paFloat32: s->frame_size *= 4; break;
    case paInt32:   s->frame_size *= 4; break;
    case paInt24:   s->frame_size *= 3; break;
    case paInt16:   s->frame_size *= 2; break;
    case paInt8:    s->frame_size *= 1; break;
    case paUInt8:   s->frame_size *= 1; break;
    default: return paSampleFormatNotSupported;

  }

  err = Pa_Initialize();
  if (err != paNoError) return err;

  return Pa_OpenDefaultStream( &s->stream
                             , 0, chan_num, sample_format, sample_rate
                             , frames_per_buffer
                             , callback
                             , s
                             );
}


void playCleanup (stream_state *s) {
  (void) Pa_CloseStream(s->stream);
  (void) Pa_Terminate();
}

PaError playStart (stream_state *s) { return Pa_StartStream(s->stream); }
PaError playStop  (stream_state *s) { return Pa_StopStream(s->stream); }
PaError playAbort (stream_state *s) { return Pa_AbortStream(s->stream); }

void playLooping (stream_state *s, int yes) { s->loop = yes; }
void playNext (stream_state *s, chunk *next) { s->next_chunk = next; }
