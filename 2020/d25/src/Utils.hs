module Utils
  ( cardPublicKey
  , doorPublicKey
  , modulus
  , revmodexp
  , modexp
  ) where

cardPublicKey, doorPublicKey, modulus :: Integer
cardPublicKey = 8335663
doorPublicKey = 8614349
modulus = 20201227

-- Reverse mod-exp. Returns x where b^x = n (mod m)
revmodexp :: (Num p, Integral t) => t -> t -> t -> p
revmodexp b n m = impl b
  where impl val = if val == n then 1 else 1 + impl ((val * b) `mod` m)

-- Modexp with binary exponentation.
modexp :: (Integral a) => a -> a -> a -> a
modexp b 0 m = 1
modexp b 1 m = b `mod` m
modexp b e m | odd e     = (modexp b (e - 1) m * b) `mod` m
             | otherwise = (modexp b (e `div` 2) m ^ 2) `mod` m
