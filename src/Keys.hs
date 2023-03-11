module Keys where

import System.Random
import System.IO
import Primes

{-
Some variables used in RSA encryption which
will be used throughout these data structures

p,q = large prime numbers

n = pq. Also the modulus for the public and private
key. Released as part of the public key

totient n = The carmichael's totient function of n.
A part of the private key

e = 16,537. This will be some integer such that 2 < e < (totient n)
and gcd(e, (totient n)) = 1 (e and totient n are coprime). This is
part of the public key

d = e^-1 `mod` (totient n). Can be computed using the
extended euclidean algorithm. d is secret.

For more information on how RSA encryption works, please check out
this link: https://en.wikipedia.org/wiki/RSA_(cryptosystem)
-}

data PrivateKey = PrivateKey
    { tot :: Integer
    , d   :: Integer
    , p1  :: Integer -- The two original primes. Not technically a part of the secret key, but need to be kept secret
    , p2  :: Integer 
    }
    deriving(Show)

data PublicKey = PublicKey 
    { n :: Integer
    , e :: Integer    
    }
    deriving(Show)

data Key = Key
    { private :: PrivateKey
    , public  :: PublicKey
    }
    deriving(Show)

-- extendGCD a b = (g,x,y)
-- g is the gcd of a and b, and ax + by = g
extendGCD :: (Integral a) => a -> a -> (a, a, a)
extendGCD a b
    | b == 0    = (abs a, signum a, 0)
    | otherwise = (g, y, x - (a `div` b) * y)
    where
    (g,x,y) = extendGCD b (a `mod` b)

-- inverse a m is the modular multiplicative inverse of a mod m.
inverse :: Integer -> Integer -> Integer
inverse a m = y `mod` m
  where
    (_,_,y) = extendGCD m a

-- We can take advantage of the fact that p1 and p2 are prime
-- and compute the totient to be lcm(p-1, p-2). Further more,
-- the best way to compute the lcm of two numbers is their
-- product divided by their gcd
carmichaelTotient :: (Integral a) => a -> a -> a
carmichaelTotient p1 p2 = div top bottom
    where
    top    = (p1-1) * (p2-1)
    bottom = gcd (p1-1) (p2-1)

createKeyPair :: (RandomGen g) => g -> Key
createKeyPair gen = Key private public
    where

    -- Need to create two generators so that it doesn't
    -- generate the same prime
    (gen1, gen2) = split gen

    -- Generate prime numbers
    p1 = largeRandomPrime gen1 :: Integer
    p2 = largeRandomPrime gen2 :: Integer

    -- Compute product
    n = p1 * p2                :: Integer

    -- Compute totient of n using primes
    tot = carmichaelTotient p1 p2

    -- Wikipedia says that this is a common value in RSA
    e = (2^16) + 1 

    -- Directly computing modular inverse of e and totient
    d = inverse e tot

    -- Create keys
    private = PrivateKey tot d p1 p2
    public  = PublicKey n e

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