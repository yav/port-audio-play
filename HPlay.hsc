{-# LANGUAGE ForeignFunctionInterface #-}
module HPlay
  ( PA
  , Chunk
  , Format
  , mallocChunk
  , freeChunk
  , chunkData
  , PAError
  , withPA
  , Channels(..)
  , playStart
  , playStop
  , playAbort
  , playNext
  , playLooping
  ) where


import Foreign
import Foreign.C
import Data.Word
import Data.Int

#include "play.h"

type PAError = Int

newtype PA f    = PA (Ptr ())
newtype Chunk f = CH (Ptr ())

mallocChunk :: PA f -> Int -> IO (Chunk f)
mallocChunk (PA p) frameNum =
  do fs <- (#peek stream_state, frame_size) p
     q <- mallocBytes ((#size chunk) + frameNum * fs)
     (#poke chunk, frame_num) q (fromIntegral frameNum :: CULong)
     return (CH q)

freeChunk :: Chunk f -> IO ()
freeChunk (CH x) = free x

chunkData :: Chunk f -> Ptr f
chunkData (CH p) = (#ptr chunk, data) p

withPA :: Format f =>
          Channels -> Int -> (Either PAError (PA f) -> IO ()) -> IO ()
withPA chans rate k = allocaBytes (#size stream_state) (mk . PA)
  where
  mk pa@(PA p) =
    do err <- playInit p (exportChannels chans)
                         (exportFormat pa)
                         (fromIntegral rate)
                         0
       if err == 0 then k (Right pa) >> playCleanup p
                   else k (Left err)

data Channels = Mono | Stereo
                deriving (Eq,Show,Read)

exportChannels :: Channels -> CULong
exportChannels c = case c of
                     Mono   -> 1
                     Stereo -> 2

class Format f        where exportFormat :: PA f -> CULong 
instance Format Word8 where exportFormat _ = (#const paUInt8)
instance Format Int8  where exportFormat _ = (#const paInt8)
instance Format Int16 where exportFormat _ = (#const paInt16)
instance Format Int32 where exportFormat _ = (#const paInt32)
instance Format Float where exportFormat _ = (#const paFloat32)

foreign import ccall unsafe
  playInit :: Ptr () -> CULong -> CULong -> CDouble
                     -> CULong -> IO PAError

foreign import ccall unsafe
  playCleanup :: Ptr () -> IO ()

foreign import ccall unsafe
  playStart :: PA f -> IO PAError

foreign import ccall unsafe
  playStop :: PA f -> IO PAError

foreign import ccall unsafe
  playAbort :: PA f -> IO PAError

foreign import ccall unsafe
  playNext :: PA f -> Chunk f -> IO ()

foreign import ccall unsafe
  playLooping :: PA f -> Bool -> IO ()



