module Keys where

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
extended euclidean algorithm. d is secret

-}

data PrivateKey = PrivateKey
    { tot :: Integer
    , d   :: Integer
    }

data PublicKey = PublicKey 
    { n :: Integer
    , e :: Integer    
    }

data Key = Key
    { private :: PrivateKey
    , public  :: PublicKey
    }