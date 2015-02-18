{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Monitor where
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import Data.Machine
import System.Hardware.Serialport
import Text.Read

serialMachine :: MonadIO m => SerialPort -> SourceT m B.ByteString
serialMachine p = repeatedly $ liftIO (recv p 8) >>= yield

-- | Break into lines separated by "\r\n" characters.
rnLines :: B.ByteString -> [B.ByteString]
rnLines bs
  | B.null t = [h]
  | otherwise = h : rnLines (B.drop 2 t)
  where (h,t) = B.breakSubstring "\r\n" bs

readMachine :: Read a => Process String a
readMachine = repeatedly $ await >>= maybe (return ()) yield . readMaybe

asciiLines :: Process B.ByteString String
asciiLines = construct $ go B.empty
  where go xs = do i <- B.filter (/= '\0') <$> await
                   let lns = rnLines $ B.append xs i
                   mapM_ (yield . B.unpack) $ init lns
                   go $ last lns

monitor :: MonadIO m => SerialPort -> SourceT m String
monitor p = serialMachine p ~> asciiLines
