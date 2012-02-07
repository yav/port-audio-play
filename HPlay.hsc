{-# LANGUAGE ForeignFunctionInterface #-}
module HPlay
  ( Audio
  , AudioError
  , withPA
  , getNext
  , getSampleRate
  , playStart
  , playStop
  , playAbort
  , playNext
  , playLooping

  , Sample
  , newSample
  , sampleFromFile
  , freeSample

  , AudioFile
  , AudioInfo(..)
  , openAudioFile
  , readAudio

  ) where


import Foreign
import Foreign.C
import Control.Monad

#include "play.h"
#include "sndfile.h"

type AudioError = CInt

newtype Audio = A (Ptr ())

withPA :: Word -> Word -> (Audio -> IO ()) -> IO (Maybe AudioError)
withPA chans rate k =
  allocaBytes (#size stream_state) $ \p ->
    do err <- playInit p (fromIntegral chans)
                         (fromIntegral rate)
                         0
       if err == 0 then k (A p) >> playCleanup p >> return Nothing
                   else return (Just err)

getNext :: Audio -> IO (Maybe Sample)
getNext (A p) =
  do q <- (#peek stream_state, next_sample) p
     return (if q == nullPtr then Nothing else Just (S q))

getSampleRate :: Audio -> IO Word
getSampleRate (A p) =
  do s <- (#peek stream_state, stream) p
     q <- pa_GetStremInfo s
     d <- (#peek PaStreamInfo, sampleRate) q
     return (floor (d :: CDouble))

foreign import ccall unsafe
  playInit :: Ptr () -> CULong -> CDouble -> CULong -> IO CInt

foreign import ccall unsafe
  playCleanup :: Ptr () -> IO ()

foreign import ccall unsafe
  playStart :: Audio -> IO AudioError

foreign import ccall unsafe
  playStop :: Audio -> IO AudioError

foreign import ccall unsafe
  playAbort :: Audio -> IO AudioError

foreign import ccall unsafe
  playNext :: Audio -> Sample -> IO ()

foreign import ccall unsafe
  playLooping :: Audio -> Bool -> IO ()

foreign import ccall unsafe "Pa_GetStreamInfo"
  pa_GetStremInfo :: Ptr () -> IO (Ptr ())


--------------------------------------------------------------------------------

data AudioInfo = AudioInfo
  { frameNum    :: !Word
  , sampleRate  :: !Word
  , channels    :: !Word
  } deriving Show

newtype AudioFile = AF (Ptr ())

openAudioFile :: FilePath -> IO (AudioFile, AudioInfo)
openAudioFile file =
  allocaBytes (#size SF_INFO) $ \i ->
    do p <- withCAString file $ \s -> sf_open s (#const SFM_READ) i
       when (p == nullPtr) $ fail $ "Failed to open " ++ file
       num   <- (#peek SF_INFO, frames)     i
       rate  <- (#peek SF_INFO, samplerate) i
       chans <- (#peek SF_INFO, channels)   i
       return ( AF p
              , AudioInfo { frameNum = fromIntegral (num :: (#type sf_count_t))
                          , sampleRate = fromIntegral (rate :: CInt)
                          , channels = fromIntegral (chans :: CInt)
                          }
              )

readAudio :: Audio -> AudioFile -> Sample -> IO Word
readAudio pa@(A a) (AF p) (S s) =
  do cs <- (#peek stream_state, channels) a
     fs <- (#peek sample, max_frames) s
     n  <- sf_read_short p ((#ptr sample, data) s) (fs * cs)
     let frames = fromIntegral (div n cs)
     (#poke sample, frame_num) s frames
     rate <- getSampleRate pa
     return (div (frames * 1000 * 1000) rate)

foreign import ccall unsafe
  sf_open :: CString -> CInt -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe
  sf_read_short :: Ptr () -> Ptr () -> (#type sf_count_t)
                                    -> IO (#type sf_count_t)


--------------------------------------------------------------------------------

newtype Sample  = S (Ptr ())

newSample :: Audio -> Word -> IO Sample
newSample (A p) n = S `fmap` c_mallocSample p (fromIntegral n)

sampleFromFile :: Audio -> FilePath -> IO (Word, Sample)
sampleFromFile a@(A pa) file =
  do (f,i) <- openAudioFile file
     cs <- (#peek stream_state, channels) pa
     unless (channels i == cs) $ fail "newSampleFile: Channel mismatch."
     s <- newSample a (frameNum i)
     l <- readAudio a f s
     return (l, s)


freeSample :: Sample -> IO ()
freeSample (S x) = free x

foreign import ccall unsafe "mallocSample"
  c_mallocSample :: Ptr () -> CULong -> IO (Ptr ())


