module Days.D07 where

import           Data.List
import           Data.List.Split
import           Utils


parse :: String -> [Int]
parse = splitOn "," .> map read

part1 :: [Int] -> Int
-- part1 xs = minimum [sum [abs (x - c) | x <- xs] | c <- [minimum xs .. maximum xs]]
part1 xs = sum [ abs (x - median) | x <- xs ]
 where
  median = if even (length xs)
    then sorted !! mid
    else ((sorted !! mid) + (sorted !! (mid - 1))) `div` 2
  mid    = length xs `div` 2
  sorted = sort xs

part2 :: [Int] -> Int
-- part2 xs = minimum [sum [dist x c | x <- xs] | c <- [minimum xs .. maximum xs]]
--   where dist x c = let a = abs (x - c) in a*(a + 1) `div` 2
part2 xs = sum [ dist x mean | x <- xs ]
 where
  mean = fromIntegral (sum xs) / fromIntegral (length xs) |> floor
  dist x c = let a = abs (x - c) in a * (a + 1) `div` 2
