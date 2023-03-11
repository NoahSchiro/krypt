module Main where

import System.Random
import Primes
import Keys

displayKey :: Key -> IO ()
displayKey k = do
    -- Print everything out
    putStrLn ("P1: " ++ (show $ p1 $ private k))
    putStrLn ("P2: " ++ (show $ p2 $ private k))
    putStrLn ("N: "  ++ (show $ n $ public k))
    putStrLn ("Totient: " ++ (show $ tot $ private k))
    putStrLn ("E: " ++ (show $ e $ public k))
    putStrLn ("D: " ++ (show $ d $ private k))

main :: IO ()
main = do

    -- Random number generator
    gen <- getStdGen

    -- Create the keys
    let key = createKeyPair gen

    writeToFile "id_rsa" key

    putStrLn "Done!"