import Prelude
import qualified Data.HashSet as S
import Data.List.Split ( splitOn )

main :: IO ()
main = readFile "input.txt" >>= print . solve . splitOn "\n\n"

solve :: [String] -> Int
solve = sum . map (count . lines)

count :: [String] -> Int
count = S.size . foldr1 S.intersection . map S.fromList
