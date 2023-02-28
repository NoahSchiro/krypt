module Main where

import System.Random
import Primes

main :: IO ()
main = do

    -- Random number generator
    gen <- getStdGen

    -- Random numbers sufficiently large for a 4098 key size.
    -- let nums = randomRs (2^2000, 2^2100) gen :: [Integer]

    let witnesses = generateWitnesses gen (100) 10

    print $ witnesses 
    