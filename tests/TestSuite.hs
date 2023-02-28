module Main where

import System.Random
import Primes

helper :: RandomGen g => g -> Integer -> Int -> Bool
helper gen x precision = probPrime x (generateWitnesses gen x precision)

diff :: [(Int, Bool)] -> [(Int, Bool)] -> [(Int, Bool)]
diff l1 l2 = filter (\(_,y) -> not y) $ zipWith (\x y -> (fst x, x==y)) l1 l2

testRange :: RandomGen g => g -> Int -> Int -> Int -> Float
testRange gen lower upper precision = (1 - (numerator / denom)) * 100
    where
    groundTruth = map (\x -> (x, primeCheck x)) [lower..upper]
    test = map (\(x,y) -> (fromIntegral x, y)) $ map (\x -> (x, helper gen (fromIntegral x) precision)) [lower..upper]
    d = diff groundTruth test
    numerator = fromIntegral $ length d
    denom = fromIntegral (upper - lower)


runMillerPrimeTest :: Int -> Int -> IO ()
runMillerPrimeTest curr precUpper = do
    g <- getStdGen

    if curr >= precUpper 
        then return ()
        else do
            let result = (testRange g 1 10000 curr)
            print ("Precision: " ++ show curr ++ "; Accuracy: " ++ show result)
            runMillerPrimeTest (curr+1) precUpper 

-- TODO: Maybe integrate HUnit instead of all this?
main :: IO ()
main = do
    {-g <- getStdGen
    let groundTruth = map (\x -> (x, primeCheck x)) [1..1000]
    let test = map (\(x,y) -> (fromIntegral x, y)) $ map (\x -> (x, helper g x 10)) [1..1000]
    let d = diff groundTruth test 
    print (testRange g 1 1000 10)
    return ()-}
    runMillerPrimeTest 1 10
