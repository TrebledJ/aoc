module Days.DXX where

import           Utils


main :: IO ()
main = defaultMain defaultFile parse part1 part2

defaultFile :: String
defaultFile = "../input/dXX.txt"

parse :: String -> a
parse = lines

part1 :: a -> Int
part1 x = 0

part2 :: a -> Int
part2 x = 0
