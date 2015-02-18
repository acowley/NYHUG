module OutputGuard where
import Data.SBV

-- | Our range detector /must/ output if the range is closer than 200.
safetyDistance :: SWord16
safetyDistance = 200

-- | This is the specified limit. We want to stay below this!
maxTimeSince :: SWord16
maxTimeSince = 10

-- | Compute the time since the last output should have been sent. An
-- output of zero from this function means an output ought to be sent
-- right /now/. The goal is say that an output should be sent if the
-- range is greater than 200, the manual override is set, or we are
-- nearing the 'maxTimeSince' limit from the specification.
f :: SWord16 -> SBool -> SWord16 -> SWord16
f range manual timeSince = 
  ite (range .> 200 ||| manual ||| timeSince .> maxTimeSince - 2)
      0
      (timeSince+1)

pf1,pf2,pf3,spec :: Predicate
pf1 = forAll ["r", "m", "t"] $ \r m t -> f r m t .< maxTimeSince

pf2 = forAll ["r", "m", "t"] $ \r m t -> 
        r .> safetyDistance ==> f r m t .== 0

pf3 = forAll ["r", "m", "t"] $ \r m t -> m ==> f r m t .== 0

spec = forAll ["r", "m", "t"] $ \r m t -> 
       let minRate = f r m t .< maxTimeSince
           minRange = r .> safetyDistance ==> f r m t .== 0
           manualOverride = m ==> f r m t .== 0
       in minRate &&& minRange &&& manualOverride

codeGen :: IO ()
codeGen = compileToC (Just "copilot-sbv-codegen") "outputGuard" $ 
          do cgGenerateDriver False
             cgGenerateMakefile False
             r <- cgInput "r"
             m <- cgInput "m"
             t <- cgInput "t"
             cgReturn $ f r m t
