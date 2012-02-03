{-# LANGUAGE ForeignFunctionInterface #-}
module HPlay
  ( Audio
  , AudioError
  , withPA
  , Channels(..)
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

withPA :: Channels -> Word -> (Either AudioError Audio -> IO ()) -> IO ()
withPA chans rate k = allocaBytes (#size stream_state) $ \p ->
                          do err <- playInit p (exportChannels chans)
                                               (fromIntegral rate)
                                               0
                             if err == 0 then k (Right (A p)) >> playCleanup p
                                         else k (Left err)

data Channels = Mono | Stereo
                deriving (Eq,Show,Read)

exportChannels :: Channels -> CULong
exportChannels c = case c of
                     Mono   -> 1
                     Stereo -> 2

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

readAudio :: Audio -> AudioFile -> Sample -> IO ()
readAudio (A a) (AF p) (S s) =
  do cs <- (#peek stream_state, channels) a
     fs <- (#peek sample, max_frames) s
     n  <- sf_read_int p ((#ptr sample, data) s) (fs * cs)
     (#poke sample, frame_num) s (div n cs)

foreign import ccall unsafe
  sf_open :: CString -> CInt -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe
  sf_read_int :: Ptr () -> Ptr () -> (#type sf_count_t) -> IO (#type sf_count_t)


--------------------------------------------------------------------------------

newtype Sample  = S (Ptr ())

newSample :: Audio -> Word -> IO Sample
newSample (A p) n = S `fmap` c_mallocSample p (fromIntegral n)

sampleFromFile :: Audio -> FilePath -> IO Sample
sampleFromFile a@(A pa) file =
  do (f,i) <- openAudioFile file
     cs <- (#peek stream_state, channels) pa
     unless (channels i == cs) $ fail "newSampleFile: Channel mismatch."
     s     <- newSample a (frameNum i)
     readAudio a f s
     return s


freeSample :: Sample -> IO ()
freeSample (S x) = free x

foreign import ccall unsafe "mallocSample"
  c_mallocSample :: Ptr () -> CULong -> IO (Ptr ())


