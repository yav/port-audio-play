import HPlay
import Control.Concurrent(threadDelay)
import Control.Monad
import System.Environment


main :: IO ()
main =
  do a : _ <- getArgs
     (f,i) <- openAudioFile a
     print i
     withPA (if channels i == 1 then Mono else Stereo) (sampleRate i) $ \mb ->
       case mb of
         Left err -> fail "Failed to open stream"
         Right p ->
           do s <- newSample p (frameNum i)
              readAudio p f s
              playNext p s
              playStart p
              threadDelay (10 * 1000 * 1000)
              playStop p
              return ()


