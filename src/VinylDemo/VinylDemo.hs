{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
-- | A brief demonstration of a bit of the functionality of the vinyl
-- package.
module Main (main) where
import Criterion.Config
import Criterion.Main
import Data.Vinyl
import Data.Vinyl.Unicode

data School = UPenn | Drexel | CMU | RPI deriving (Eq,Ord,Show)

type Name = "name" ::: String
type Affil = "affiliation" ::: School

name :: Name
name = Field

school :: Affil
school = Field

roboticists :: [PlainRec [Name, Affil]]
roboticists = [ name =: "Anthony" <+> school =: UPenn
              , name =: "Ani"     <+> school =: Drexel
              , name =: "Ben"     <+> school =: CMU
              , name =: "Jeff"    <+> school =: RPI ]

names :: (Name ∈ fields) => [PlainRec fields] -> [String]
names = map (rGet name)

schools :: (Affil ∈ fields) => [PlainRec fields] -> [PlainRec '[Affil]]
schools = map cast

-- NOTE: This doesn't type check! 'schools' leaves us with records
-- with only an 'Affil' field, while 'names' accepts any record type
-- that has a 'Name' field. The type error indicates that the type
-- checker looked at the return type of 'schools', decided that @Affil
-- /= Name@, then couldn't find 'Name' among the remaining
-- fields... because there were no remaining fields, i.e. the empty
-- list.
-- checked = names . schools

--------------------------------------------------------------------------------
-- Let's compare the runtime efficiency of the vinyl representation as
-- compared to representations using tuples and stadard Haskell
-- records.

roboticistPairs :: [(String,School)]
roboticistPairs = [ ("Anthony", UPenn)
                  , ("Ani", Drexel)
                  , ("Ben", CMU)
                  , ("Jeff", RPI) ]

data Roboticist = Roboticist { rname   :: !String
                             , _raffil :: !School  }

roboticists' :: [Roboticist]
roboticists' = [ Roboticist "Anthony" UPenn
               , Roboticist "Ani" Drexel
               , Roboticist "Ben" CMU
               , Roboticist "Jeff" RPI ]

main :: IO ()
main = do if tupleFun roboticistPairs == vinylFun roboticists &&
             vinylFun roboticists == recordFun roboticists'
            then putStrLn "Variations are in agreement"
            else putStrLn "Results are inconsistent"
          defaultMainWith (defaultConfig {cfgSamples = ljust 1000})
                          (return ())
            [ bench "tuple" $ whnf tupleFun roboticistPairs
            , bench "vinyl" $ whnf vinylFun roboticists
            , bench "record" $ whnf recordFun roboticists' ]
  where tupleFun = sum . map (length . fst)
        vinylFun = sum . map length . names
        recordFun = sum . map (length . rname)
