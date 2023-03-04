module Main where

import System.Random
import Primes
import Keys

main :: IO ()
main = do

    -- Random number generator
    gen <- getStdGen

    -- Create the keys
    let key = createKeyPair gen

    -- Print everything out
    putStrLn ("P1: " ++ (show $ p1 $ private key))
    putStrLn ("P2: " ++ (show $ p2 $ private key))
    putStrLn ("N: "  ++ (show $ n $ public key))
    putStrLn ("Totient: " ++ (show $ tot $ private key))
    putStrLn ("E: " ++ (show $ e $ public key))
    putStrLn ("D: " ++ (show $ d $ private key))