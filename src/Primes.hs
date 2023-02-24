module Primes where


{-
This is a prime checker which is not suitable for
encryption purposes because it is slow. However,
we will use it in testing as a "ground truth" of
what is prime. Even though this is slow compared
to the method we will be using, I will still try
to write it to be resonably fast
-}
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
