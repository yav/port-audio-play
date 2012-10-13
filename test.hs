import Sound.Play
import Control.Concurrent
import System.Environment


main :: IO ()
main =
  do f1 : f2 : _ <- getArgs
     s1 <- sampleFromFile f1
     s2 <- sampleFromFile f2

     withPortAudio 1 11025 $ \pa ->

        do x <- newEmptyMVar
           playSample pa s1
           _ <- forkIO $
             do threadDelay 1000000
                playSample pa s2
                threadDelay (sampleLenMillis s2)
                putMVar x ()
           takeMVar x




