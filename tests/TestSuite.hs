module Main where

import Primes

primeRange start stop = [primeCheck x | x <- [start..stop]]


-- TODO: Maybe integrate HUnit instead of all this?
main :: IO ()
main = do
    print $ map (\x -> (x, primeCheck x)) [1..10] 