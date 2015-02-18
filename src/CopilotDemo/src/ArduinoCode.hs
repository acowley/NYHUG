import Language.Copilot
import qualified Copilot.Compile.SBV as S
import qualified Prelude as P
import qualified OutputGuard as OG

ledPin, rangePin, buttonPin :: Int16
ledPin    = 13
rangePin  = 0
buttonPin = 8

outputGuard :: Stream Word16 -> Stream Bool -> Stream Word16 -> Stream Word16
outputGuard r m p = externFun "outputGuard" [arg r, arg m, arg p] Nothing

analogRead :: Int16 -> Stream Word16
analogRead p = [0] ++ externFun "analogRead" [arg $ constI16 p] Nothing

digitalRead :: Int16 -> Stream Bool
digitalRead p = [False] ++ externFun "digitalRead" [arg $ constI16 p] Nothing

-- We have to be careful about only depending on yesterday's values
-- for today's logic!
rangeReporter :: Spec
rangeReporter = do trigger "serialPrint" go [arg range]
                   trigger "digitalWrite" true [arg $ constI16 ledPin, arg go]
  where range = analogRead rangePin
        manual = digitalRead buttonPin
        prev = [2] ++ outputGuard range manual prev
        go = [False] ++ prev == 0

-- Compile 'rangeReporter' to C using the SBV backend. This creates a
-- @step@ function that we want to call in our main loop.
toSBV :: IO ()
toSBV = reify rangeReporter >>= S.compile S.defaultParams

setup :: String
setup = P.unlines [ "#include <Arduino.h>"
                  , "#include <stdint.h>"
                  , "void setup() {"
                  , "  pinMode(" P.++ show ledPin P.++ ", OUTPUT);"
                  , "  pinMode(" P.++ show rangePin P.++ ", INPUT);"
                  , "  pinMode(" P.++ show buttonPin P.++", INPUT);"
                  , "  Serial.begin(9600);"
                  , "}" ]

loop :: String
loop = P.unlines [ "extern \"C\" void step(void);"
                 , "extern \"C\" void serialPrint(uint16_t x) {"
                 , "  Serial.println(x);"
                 , "}"
                 , "void loop() {"
                 , "  step();"
                 , "  delay(100);"
                 , "}" ]

fixIno :: IO ()
fixIno = writeFile "arduino/src/sketch.cpp" (setup P.++ loop)

main :: IO ()
main = OG.codeGen >> toSBV >> fixIno
