module Primes(
    primeCheck,
    isPrimePure,
    isPrime,
    largeRandomPrime
) where

import System.Random

{- This is a prime checker which is not suitable for
encryption purposes because it is slow. However,
we will use it in testing as a "ground truth" of
what is prime. Even though this is slow compared
to the method we will be using, I will still try
to write it to be resonably fast-}
primeCheck :: (Integral a) => a -> Bool
primeCheck x
    | x < 2            = False
    | x == 2           = True
    | (x `mod` 2) == 0 = False
    | otherwise        = helper x 3
    where

    --Upper bound of possible divisiors
    limit = ceiling $ sqrt $ fromIntegral x

    -- Checks every divisor range 3 sqrt(x), increment by 2
    helper x div
        | div >= limit       = True
        | (x `mod` div) == 0 = False
        | otherwise          = helper x (div+2)

-- Binary exponenciation
power :: (Integral a) => a -> a -> a -> a 
power a 1 n = a
power a e n = (a^(e `mod` 2) * power (a*a `mod` n) (e `div` 2) n) `mod` n 

-- Given x returns (y,z) where x = 2^y * z and z odd
twoPowersFact :: (Integral a) => a -> (a,a)
twoPowersFact 0 = (0,0)
twoPowersFact m = helper (0, abs m)
    where
    helper (s, n)
        | even n    = helper (s+1, n `div` 2)
        | otherwise = (s,n)

millerRabin :: (Integral a) => a -> a -> Bool
millerRabin num a = base == 1 || test base (fst fact) 0 num
    where

    fact = twoPowersFact (num-1)

    base = power a (snd fact) num

    test :: (Integral a) => a -> a -> a -> a -> Bool
    test b s r n
        | r == s           = b == n-1
        | b == 1 && r /= 0 = False
        | otherwise        = b == n-1 || test (power b 2 n) s (r+1) n

-- isPrimePure possiblePrime [list of testBases]
isPrimePure :: (Eq a, Integral a) => a -> [a] -> Bool 
isPrimePure can wit
    | can < 100 = primeCheck can -- For low numbers, just use the normal test
    | null wit  = False -- If we run out of witnesses
    | otherwise = all (\ p -> p >= can || (can `mod` p) /= 0) primes
               && foldr ((&&) . millerRabin can) True wit
    where
    primes = [3,5,7,11,13,17,19,23,29,31]

-- Wrapper primality test; generates the witnesses 
-- and passes them off to the primary primality test
isPrime :: (RandomGen g, Integral a, Random a) => g -> a -> Bool
isPrime gen can = isPrimePure can witnesses
    where
    -- We use 20 witnesses, but this is arbitrary
    witnesses = take 20 $ randomRs (2,can-2) gen

largeRandomPrime :: (RandomGen g, Integral a, Random a) => g -> a
largeRandomPrime gen
    | even randomNum        = largeRandomPrime nextGen
    | isPrime gen randomNum = randomNum
    | otherwise             = largeRandomPrime nextGen
    where 
    (randomNum, nextGen) = randomR (2^2000, 2^2100) gen
