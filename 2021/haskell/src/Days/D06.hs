-- Algorithm: This uses the matrix method which you may have seen elsewhere.
-- This method is rather efficient and takes O(log(n)) time. Essentially, we
-- devise a matrix which transforms our state from step N to step N + 1. Our
-- state is a list of 9 elements: the number of fish with timers at 0 .. 8.
-- Using binary exponentiation, we can compute the transform from step 1 to step
-- N and apply it to our state in one go.

module Days.D06 where

import           Data.List.Split
import qualified Data.Matrix                   as M
import           Utils


main :: IO ()
main = defaultMain defaultFile parse part1 part2

defaultFile :: String
defaultFile = "../input/d06.txt"

parse :: String -> [Int]
parse s = map read $ splitOn "," s

transform :: M.Matrix Int
transform = M.fromLists
  [ [0, 1, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 1, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 1, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 1, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 1, 0, 0]
  , [1, 0, 0, 0, 0, 0, 0, 1, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 1]
  , [1, 0, 0, 0, 0, 0, 0, 0, 0]
  ]

expTransform :: Int -> M.Matrix Int
expTransform 1 = transform
expTransform days
  | even days
  = let tmp = expTransform (days `div` 2) in tmp `M.multStd` tmp
  | otherwise
  = let tmp = expTransform (days `div` 2)
    in  tmp `M.multStd` tmp `M.multStd` transform

countLanternfish :: [Int] -> [Int]
countLanternfish xs = [ count (== i) xs | i <- [0 .. 8] ]

simulate :: Int -> [Int] -> Int
simulate days input = sum $ M.toList $ expTransform days `M.multStd` initCount
  where initCount = M.fromList 9 1 (countLanternfish input)

part1 :: [Int] -> Int
part1 = simulate 80

part2 :: [Int] -> Int
part2 = simulate 256
