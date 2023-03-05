module Main where

import System.Random
import System.IO
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

writeToFile :: String -> Key -> IO ()
writeToFile filePath (Key private public) = do

    let publicFilePath = (filePath ++ ".pub")

    -- Write out private key first
    privatFD <- openFile filePath WriteMode

    hPutStrLn privatFD (show $ tot private)
    hPutStrLn privatFD (show $ d private)
    hPutStrLn privatFD (show $ p1 private)
    hPutStrLn privatFD (show $ p2 private)

    -- Done with that file
    hClose privatFD

    -- Write out the public key next
    publicFD <- openFile publicFilePath WriteMode

    hPutStrLn publicFD (show $ n public)
    hPutStrLn publicFD (show $ e public)

    -- Shut down
    hClose publicFD

readFromFile :: String -> IO Key 
readFromFile filePath = do
     
    let publicFilePath = (filePath ++ ".pub")

    -- Open, get contents, close
    privatFD <- openFile filePath ReadMode
    privatContents <- hGetContents privatFD
    let privatParsed = map (read) $ lines privatContents 

    -- Parse
    let t    = privatParsed !! 0 :: Integer
    let d    = privatParsed !! 1 :: Integer
    let pOne = privatParsed !! 2 :: Integer
    let pTwo = privatParsed !! 3 :: Integer

    -- Construct private key 
    let privatKey = PrivateKey t d pOne pTwo

    -- Open, get contents, close
    publicFD <- openFile publicFilePath ReadMode
    publicContents <- hGetContents publicFD
    let publicParsed = map (read) $ lines publicContents

    -- Parse
    let n = publicParsed !! 0 :: Integer
    let e = publicParsed !! 1 :: Integer

    -- Construct public key
    let publicKey = PublicKey n e

    -- Return
    return (Key privatKey publicKey)

main :: IO ()
main = do

    -- Random number generator
    gen <- getStdGen

    -- Create the keys
    --let key = createKeyPair gen

    key <- readFromFile "id_rsa"

    displayKey key