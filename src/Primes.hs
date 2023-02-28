module Primes where

import System.Random

{- This is a prime checker which is not suitable for
encryption purposes because it is slow. However,
we will use it in testing as a "ground truth" of
what is prime. Even though this is slow compared
to the method we will be using, I will still try
to write it to be resonably fast-}
primeCheck :: Int -> Bool
primeCheck x
    | x < 2            = False
    | x == 2           = True
    | (x `mod` 2) == 0 = False
    | otherwise        = helper x 3
    where

    limit :: Int -- Cache the upper bound of possible divisors
    limit = ceiling $ sqrt $ fromIntegral x

    -- Checks every divisor range 3 sqrt(x), increment by 2
    helper :: Int -> Int -> Bool
    helper x div
        | div >= limit       = True
        | (x `mod` div) == 0 = False
        | otherwise          = helper x (div+2)

{-Given a the candidate prime and the precision (number of
witnesses), generates a list of witnesses-}
generateWitnesses :: RandomGen g => g -> Integer -> Int -> [Integer]
generateWitnesses gen num prec = take prec $ randomRs (2, num-2) gen

{-Returns whether or not num is a probable prime given a single witness
Note that witness must be a number [2..num-2]-}
probPrimeOneWitness :: Integer -> Integer -> Bool
probPrimeOneWitness num witness
    | x == 1 || x == num-1 = True
    | otherwise            = generalCase d x 
    where
 
    getD :: Integer -> Integer
    getD possibleD
        | odd possibleD = possibleD 
        | otherwise     = getD (div possibleD 2)

    d :: Integer
    d = getD (num-1)

    power :: Integer -> Integer -> Integer -> Integer
    power a' d' n' = (a'^d') `mod` n'

    x :: Integer
    x = power witness d num

    -- General case will seek to keep squaring x until
    -- 1. d is greater than or equal to n-1
    -- 2. x^2 % n is not 1
    -- 3. x^2 % n is not n-1
    generalCase :: Integer -> Integer -> Bool
    generalCase d' x'
        | d' >= num-1 = False
        | x' == 1     = False
        | x' == num-1 = True
        | otherwise   = generalCase (d'*2) ((x'^2) `mod` num)

{-Tests the primality of a single number given
that number and a list of witnesses-}
probPrime :: Integer -> [Integer] -> Bool
probPrime num witnesses
    | num < 100      = primeCheck (fromIntegral num) -- If num is small, just do the normal check
    | null witnesses = True -- If no witnesses are left then it passes
    | not (probPrimeOneWitness num (head witnesses)) = False -- Test for the first witness
    | otherwise      = probPrime num (tail witnesses)

{-Given a infinite list of candidate primes and a
list of "witnesses", returns a (probable) prime (using
miller-rabin primality) and the remainder of the list-}
millerRabin :: [Integer]  -> [Integer] -> (Integer, [Integer])
millerRabin nums witnesses
    | even $ candidate              = millerRabin remainder witnesses --Ensure it is odd
    | probPrime candidate witnesses = (candidate, remainder) -- Main test
    | otherwise                     = millerRabin remainder witnesses -- If test fails, try the next num
    where
    
    candidate :: Integer
    candidate = head nums

    remainder :: [Integer]
    remainder = tail nums