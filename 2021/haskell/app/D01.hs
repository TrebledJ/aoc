module D01 where

import           Utils


main :: IO ()
main = defaultMain defaultFile parse part1 part2

defaultFile :: String
defaultFile = "../input/d01.txt"

parse :: String -> [Int]
parse = map read . lines

part1 :: [Int] -> Int
part1 xs = length $ filter (uncurry (<)) $ zip xs (tail xs)

part2 :: [Int] -> Int
part2 xs =
  part1 $ map (\(a, b, c) -> a + b + c) $ zip3 xs (tail xs) (tail $ tail xs)
