import Sound.Play
import Control.Concurrent
import System.Environment


main :: IO ()
main =
  do f1 : f2 : _ <- getArgs
     s1 <- sampleFromFile f1
     print $ (sampleRate s1, sampleChans s1)

     s2 <- sampleFromFile f2

     withPortAudio 2 44100 $ \pa ->

        do x <- newEmptyMVar
           playSample pa s1
           threadDelay (sampleLenMillis s1 + 1500000)
           playSample pa s2
           threadDelay (sampleLenMillis s2)




