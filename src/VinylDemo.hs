{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
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

--checked = names . schools

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
main = do if pf roboticistPairs == vf roboticists &&
             vf roboticists == rf roboticists'
            then putStrLn "Variations are in agreement"
            else putStrLn "Results are inconsistent"
          defaultMainWith (defaultConfig {cfgSamples = ljust 1000})
                          (return ())
            [ bench "list" $ whnf pf roboticistPairs
            , bench "vinyl" $ whnf vf roboticists
            , bench "record" $ whnf rf roboticists' ]
  where pf = sum . map (length . fst)
        vf = sum . map length . names
        rf = sum . map (length . rname)
