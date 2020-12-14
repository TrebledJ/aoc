import Prelude
import Data.List (sort)
import qualified Data.Vector as V

main :: IO ()
main = readFile "input.txt" >>= print . solve . map read . lines

-- DP Solution.
solve :: [Int] -> Int
solve = compute . sort

compute :: [Int] -> Int
compute xs = V.last $ foldl computeIndex (V.replicate n 0) (0:xs)
  where n = last xs + 1

-- Tribonacci over the numbers.
computeIndex :: V.Vector Int -> Int -> V.Vector Int
computeIndex v i = v V.// [(i, val)]
  where val
          | i < 2     = 1
          | i == 2    = (v V.! 0) + (v V.! 1)
          | otherwise = sum $ map ((v V.!) . (i-)) [1..3]
