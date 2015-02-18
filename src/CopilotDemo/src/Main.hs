import Control.Applicative
import Data.Foldable (foldMap)
import Data.List (isPrefixOf)
import Data.Machine
import Data.Monoid
import System.FilePath.Find (find, depth, (<?), fileName)
import System.Hardware.Serialport
import Monitor
import Logger

findArduino :: IO (Maybe FilePath)
findArduino = getFirst . foldMap (First . Just) <$> 
              find (depth <? 1) (isPrefixOf "tty.usbserial" <$> fileName) "/dev"

duino :: FilePath -> IO ()
duino f = do putStrLn $ "Monitoring Arduino at "++f
             withSerial f defaultSerialSettings $ 
               runT_ . ((logger <~ readMachine) <~) . monitor

main :: IO ()
main = findArduino >>= maybe notFound duino
  where notFound = error "Couldn't find Arduino serial port!"
