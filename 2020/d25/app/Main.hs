module Main where

import           Utils

main :: IO ()
main = do
  let cardLoopSize  = revmodexp 7 cardPublicKey modulus
      encryptionKey = modexp doorPublicKey cardLoopSize modulus

  putStr "Card Loop Size: "
  print cardLoopSize
  putStr "Encryption Key: "
  print encryptionKey

  return ()
