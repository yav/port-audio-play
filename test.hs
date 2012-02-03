import HPlay
import Foreign
import Foreign.C
import Data.WAVE
import Control.Concurrent(threadDelay)
import System.Environment
import Data.Int


getSamples :: PA Int32 -> FilePath -> IO (Chunk Int32)
getSamples pa f =
  do w <- getWAVEFile f
     let Just n = waveFrames (waveHeader w)
     putStrLn ("Got " ++ show n ++ " frames.");
     p <- mallocChunk pa n
     let ss0 = map head (waveSamples w)
         mix (x : xs) = x : x : mix xs
         mix [] = []
         ss = mix ss0

     pokeArray (chunkData p) ss
     return p


main :: IO ()
main =
  do a : _ <- getArgs
     withPA Stereo 11025 $ \mb ->
       case mb of
         Left err -> fail "Failed to open stream"
         Right p ->
           do playNext p =<< getSamples p a
              playLooping p True
              playStart p
              threadDelay (10 * 1000 * 1000)
              playStop p
              return ()

