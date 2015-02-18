{-# LANGUAGE GADTs, OverloadedStrings #-}
module Logger (logger) where
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Machine
import Data.Maybe
import Data.Void
import System.Remote.Monitoring
import qualified System.Remote.Gauge as G

-- | Build a composite logging process by feeding individual loggers
-- in lockstep.
mkLogger :: (Functor m, MonadIO m) => [ProcessT m Int Void] -> ProcessT m Int r
mkLogger loggers = encased $ Await (MachineT . aux) Refl (mkLogger loggers)
  where aux x = mapM (runMachineT >=> feed) loggers
                >>= runMachineT . mkLogger . catMaybes
          where feed (Await f Refl _) = Just . encased <$> runMachineT (f x)
                feed (Yield _ _)      = error "Impossible to yield a Void!"
                feed Stop             = return Nothing

-- | Print ranges greater than 100 to stdout.
logStdOut :: MonadIO m => ProcessT m Int Void
logStdOut = repeatedly $
            do r <- await
               when (r > 100) (liftIO . putStrLn $ "Range = "++show r)

-- | Log all values to a file on disk in batches of 50.
logCSV :: MonadIO m => FilePath -> ProcessT m Int Void
logCSV f = buffered 50
         ~> construct (liftIO (writeFile f "ranges\n") >> forever go)
  where go = await >>= liftIO . flushLines
        flushLines = appendFile f . (++"\n") . intercalate "\n" . map show

-- | Start up an EKG monitor, and update a 'Gauge' with the current
-- range.
logEKG :: MonadIO m => ProcessT m Int Void
logEKG = construct $ liftIO setup >>= forever . go
  where setup = forkServer "localhost" 8000 >>= getGauge "Range"
        go g = await >>= liftIO . G.set g

-- | The composite logger for range information.
logger :: (Functor m, MonadIO m) => ProcessT m Int r
logger = mkLogger [logStdOut, logCSV "ranges.csv", logEKG]
