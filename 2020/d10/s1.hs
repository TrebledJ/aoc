import Prelude
import Data.List (sort)

main :: IO ()
main = readFile "input.txt" >>= print . solve . map read . lines

solve :: [Int] -> Int
solve = uncurry (*) . compute . sort

compute :: [Int] -> (Int, Int)
compute xs = (count (== 3) diffs + 1, count (== 1) diffs)
  where diffs = zipWith (-) xs (0:xs)
        count p = length . filter p
