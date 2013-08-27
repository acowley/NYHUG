-- Compiled with:
-- ghc -no-user-package-db -package-db ../.cabal-sandbox/x86_64-osx-ghc-7.6.1-packages.conf.d -O2 -fforce-recomp -ddump-simpl-stats Inplace.hs
module Main where
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Time.Clock
import System.IO.Unsafe
import Text.Printf

-- When working with the FFI, we will often have a 'Ptr' to our data.
type MyData = IORef Int

myData :: Int -> MyData
myData = unsafePerformIO . newIORef
{-# NOINLINE myData #-}

showData :: MyData -> String
showData = show . unsafePerformIO . readIORef
{-# NOINLINE showData #-}

-- The operation we're calling through the FFI mutates its argument
-- in-place.
foreignOp :: MyData -> IO ()
foreignOp = flip modifyIORef' succ

-- So when we want to provide a pure interface, we have to manually
-- create a copy of the input.
dupData :: MyData -> IO MyData
dupData = readIORef >=> newIORef

op :: (MyData -> IO ()) -> MyData -> MyData
op f x = unsafePerformIO $
         do x' <- threadDelay 1000000 >> dupData x
            readIORef x >>= putStrLn . ("Duplicated "++) . show
            f x'
            return x'
{-# NOINLINE op #-}

-- Now we can get a handle on a pure version of our operation that
-- prevents in-place mutation.
inc :: MyData -> MyData
inc = op foreignOp
{-# INLINE inc #-}

add :: MyData -> MyData -> MyData
add x y = unsafePerformIO $
          (+) <$> readIORef x <*> readIORef y >>= newIORef
{-# NOINLINE add #-}

getVal :: MyData -> Int
getVal = unsafePerformIO . readIORef
{-# NOINLINE getVal #-}

timed :: IO a -> IO a
timed m = do t <- getCurrentTime
             r <- m
             t' <- getCurrentTime
             let dt = realToFrac (t' `diffUTCTime` t) :: Double
             printf "Calculation took %.1fs\n" dt
             return r

main :: IO ()
main = do timed $
            do let x = myData 3
               putStrLn $ "x = " ++ showData x
               let y = inc . inc $ x
               putStrLn $ "y = " ++ showData y
          putStrLn ""
          timed $
            do let p = myData 7
                   q = myData 4
               putStrLn $ "p,q = " ++ showData p ++ ", " ++ showData q
               let y = inc . inc $ p
                   z = inc . inc . inc $ q
               mapM (async . evaluate) [y,z] >>= mapM_ wait
               putStrLn $ "y + z = " ++ showData (add y z)

{-# RULES "op/compose" forall f g. op f . op g = op (\x -> g x >> f x) #-}