module Main where

import System.Random
import Primes

testNormalRange :: (Integral a) => a -> a -> [a]
testNormalRange lower upper = [x | x <- [lower..upper], primeCheck x]

testMRRange :: (Eq a, Integral a) => a -> a -> [a] -> [a]
testMRRange lower upper wit = [x | x <- [lower..upper], isPrimePure x wit]

diff :: [(Int, Bool)] -> [(Int, Bool)] -> [(Int, Bool)]
diff l1 l2 = filter (\(_,y) -> not y) $ zipWith (\x y -> (fst x, x==y)) l1 l2

-- TODO: Maybe integrate HUnit instead of all this?
main :: IO ()
main = do
    g <- getStdGen
    {-
    let witnesses = [fst $ randomR (1, 100) g] :: [Integer]

    let normal = testNormalRange 1 1000
    let milrab = testMRRange 1 1000 witnesses :: [Integer]
    print $ length normal
    print $ length milrab -}
    let out = largeRandomPrime g :: Integer
    print out 