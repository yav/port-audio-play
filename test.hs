import HPlay
import Control.Concurrent(threadDelay)
import Control.Monad
import System.Environment


main :: IO ()
main =
  do a : _ <- getArgs
     (f,i) <- openAudioFile a
     print i
     mb <- withPA (channels i) (sampleRate i) $ \p ->
           do let buf = 2 * sampleRate i    -- buffers contain 2 seconds
              s1 <- newSample p buf
              s2 <- newSample p buf
              s3 <- newSample p buf

              _  <- readAudio p f s1
              playNext p s1
              _ <- playStart p
              loadNext p f s2 s3 s1
              _ <- playStop p
              return ()

     case mb of
       Just err -> fail ("Failed to open stream: " ++ show err)
       Nothing  -> return ()




loadNext :: Audio -> AudioFile -> Sample -> Sample -> Sample -> IO ()
loadNext p file s1 s2 s3 =
  do putStrLn "loadNext"
     l1 <- readAudio p file s1
     print l1
     wait p -- does a bit of extra waiting at the end.
     unless (l1 == 0) $ do playNext p s1
                           loadNext p file s2 s3 s1


wait :: Audio -> IO ()
wait p = do mb <- getNext p
            case mb of
              Just _ -> -- next is loaded, so we have about 2 seconds to go
                        -- (maybe more as there's also the currently sample)
                        -- of course, we might be _just_ changing so to be
                        -- safe we wait for 1 second
                        threadDelay (1 * 1000 * 1000) >> wait p
              _      -> return ()


