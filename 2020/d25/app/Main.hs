module Main where

import           Utils

main :: IO ()
main = do
  let cardLoopSize  = rme 7 cardPublicKey modulus
      encryptionKey = modexp doorPublicKey cardLoopSize modulus

  putStr "Card Loop Size: "
  print cardLoopSize
  putStr "Encryption Key: "
  print encryptionKey

  return ()

-- Reverse mod-exp. Returns x where b^x = n (mod m)
rme b n m = rmeGo b
  where rmeGo val = if val == n then 1 else 1 + rmeGo ((val * b) `mod` m)

-- Modexp with binary exponentation.
modexp b 0 m = 1
modexp b 1 m = b `mod` m
modexp b e m | odd e     = (modexp b (e - 1) m * b) `mod` m
             | otherwise = (modexp b (e `div` 2) m ^ 2) `mod` m

