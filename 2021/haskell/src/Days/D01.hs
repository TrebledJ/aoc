module Days.D01 where

import           Utils


parse :: String -> [Int]
parse = lines .> map read

part1 :: [Int] -> Int
part1 xs = xs |> zip (tail xs) 
              |> filter (uncurry (>)) 
              |> length

part2 :: [Int] -> Int
part2 xs = xs |> zip3 (xs |> tail) (xs |> tail |> tail) 
              |> map (\(a, b, c) -> a + b + c) 
              |> part1
