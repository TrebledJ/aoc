module Days.D01 where

import           Utils


parse :: String -> [Int]
parse = map read . lines

part1 :: [Int] -> Int
part1 xs = length $ filter (uncurry (<)) $ zip xs (tail xs)

part2 :: [Int] -> Int
part2 xs =
  part1 $ map (\(a, b, c) -> a + b + c) $ zip3 xs (tail xs) (tail $ tail xs)
