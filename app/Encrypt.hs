module Main where

import System.Environment
import System.IO
import Keys
import Primes

usage :: String
usage = 
    "USAGE:\n\
    \   -h / -help:     prints this message\n\
    \   [input]:        file you want to encrypt\n\
    \   [key]:          key location\n"

parseArgs :: [String] -> (String, String)
parseArgs args
    | len == 2  = (target, key)
    | otherwise = ("", "")
    where
        len    = length args
        target = args !! 0
        key    = args !! 1

encryptByte :: PublicKey -> [Integer] -> [Integer]
encryptByte (PublicKey n e) xs = map (\x -> power x e n) xs

main :: IO ()
main = do
    args <- getArgs
    let (target, key) = parseArgs args

    if target == "" || key == "" 
        then putStr usage 
        else do

            -- Get key
            key <- readFromFile key
            let pub = public key

            -- Get text
            fd <- openFile target ReadMode
            contents <- hGetContents fd
            let contentBytes = map (fromIntegral . fromEnum) contents :: [Integer]
            let encrypted    = encryptByte pub contentBytes

            out <- openFile "output.txt" WriteMode
            hPutStr out (concatMap (\x -> ((show x) ++ "\n")) encrypted)