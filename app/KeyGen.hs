module Main where

import System.Random
import Primes

getPrimes :: (RandomGen g) => g -> (Integer, Integer)
getPrimes gen = (p, q)
    where
    (genOne, genTwo) = split gen
    p = largeRandomPrime genOne
    q = largeRandomPrime genTwo

main :: IO ()
main = do

    -- Random number generator
    gen <- getStdGen

    -- Get two large random primes
    let (p, q) = getPrimes gen

    print p
    print "\n"
    print q

    -- Next would be to do some prime stuff / 