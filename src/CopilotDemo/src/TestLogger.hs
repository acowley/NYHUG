import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad.IO.Class
import Data.Machine
import Logger

canned :: [Int]
canned = cycle $ map toRng [0..179::Int]
  where toRng = floor . (* 1024) . sin . toRad . fromIntegral
        toRad x = x * pi / 180 :: Double

delayed :: MonadIO m => Int -> ProcessT m a a
delayed ms = repeatedly $ liftIO (threadDelay (ms * 1000)) >> await >>= yield

main :: IO ()
main = do t <- forkIO . runT_ $ supply canned (delayed 100) ~> logger
          _ <- getLine
          killThread t
