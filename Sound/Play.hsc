{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.Play
  ( PortAudio
  , withPortAudio
  , sampleFromFile
  , playSample

  , Sample
  , sampleFrames
  , sampleRate
  , sampleChans
  , sampleLenMillis
  ) where


import Foreign
import Foreign.C
import Control.Monad
import Control.Exception(finally)
import Control.Concurrent

#include "play.h"
#include "sndfile.h"

data PortAudio = A (MVar ()) (Ptr ())

withPortAudio :: Word -> Word -> (PortAudio -> IO ()) -> IO ()
withPortAudio chans rate k =
  allocaBytes (#size stream_state) $ \p ->
    do err <- playInit p (fromIntegral chans)
                         (fromIntegral rate)
                         0
       lock <- newMVar ()
       if err == 0 then k (A lock p) `finally` playCleanup p
                   else fail $ "Failed to open audio device: " ++ show err

playSample :: PortAudio -> Sample -> IO ()
playSample (A lock pa) (Sample fs _ _ d) =
  withMVar lock    $ \_ ->
  withForeignPtr d $ \p -> playFrames pa fs p

foreign import ccall unsafe
  playInit :: Ptr () -> CULong -> CDouble -> CULong -> IO CInt

foreign import ccall unsafe
  playFrames :: Ptr () -> CULong -> Ptr CShort -> IO ()

foreign import ccall unsafe
  playCleanup :: Ptr () -> IO ()


--------------------------------------------------------------------------------

data Sample = Sample !CULong !Word !Word (ForeignPtr CShort)

sampleFrames :: Sample -> Word
sampleFrames (Sample fs _ _ _) = fromIntegral fs

sampleRate :: Sample -> Word
sampleRate (Sample _ r _ _) = r

sampleChans :: Sample -> Word
sampleChans (Sample _ _ c _) = c

sampleLenMillis :: Sample -> Int
sampleLenMillis s = fromIntegral (1000000 * sampleFrames s `div` sampleRate s)


sampleFromFile :: FilePath -> IO Sample
sampleFromFile path =
  allocaBytes (#size SF_INFO) $ \i ->
    do (#poke SF_INFO, format) i (0 :: CInt)
       file  <- withCAString path $ \s -> sf_open s (#const SFM_READ) i
       when (file == nullPtr) $ fail $ "Failed to open " ++ path
       num   <- (#peek SF_INFO, frames)     i
       rate  <- (#peek SF_INFO, samplerate) i
       chans <- (#peek SF_INFO, channels)   i
       let items = num * chans
       samp  <- mallocForeignPtrArray (fromIntegral items)
       _ <- withForeignPtr samp $ \s ->
              sf_read_short file s (fromIntegral items)
       return (Sample (fromIntegral (num :: CInt))
                      (fromIntegral (rate :: CInt))
                      (fromIntegral (chans :: CInt))
                      samp)

foreign import ccall unsafe
  sf_open :: CString -> CInt -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe
  sf_read_short :: Ptr () -> Ptr CShort -> (#type sf_count_t)
                                        -> IO (#type sf_count_t)




