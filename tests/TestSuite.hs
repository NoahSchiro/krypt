module Main where

import System.Random
import Primes

testNormalRange :: (Integral a) => a -> a -> [a]
testNormalRange lower upper = [x | x <- [lower..upper], primeCheck x]

testMRRange :: (Eq a, Integral a) => a -> a -> [a] -> [a]
testMRRange lower upper wit = [x | x <- [lower..upper], isPrimePure x wit]

isReallyPrime :: (Integral a) => [a] -> [a]
isReallyPrime xs = [x | x <- xs, not $ primeCheck x]

-- TODO: Maybe integrate HUnit instead of all this?
main :: IO ()
main = do
    g <- getStdGen

    let witnesses = take 10 $ randomRs (1, 100) g :: [Integer]

    --let normal = testNormalRange 100 200 
    let milrab = testMRRange 1 1000000 witnesses :: [Integer]
    print $ length (isReallyPrime milrab)
    
    --let out = largeRandomPrime g :: Integer
    --print out 