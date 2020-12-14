import Prelude
import qualified Data.HashSet as S
import Data.List (inits)

window = 25

main :: IO ()
main = readFile "input.txt" >>= print . solve . map read . lines

solve :: [Int] -> Int
solve xs = maximum nums + minimum nums
  where nums = head $ findGenericSumTo xs weakness
        weakness = findWeakNum xs (drop window xs)

findWeakNum :: [Int] -> [Int] -> Int
findWeakNum _ [] = error "Could not find the weak number."
findWeakNum xs (y:ys) = if not $ hasSummands (take window xs) y 
                        then y
                        else findWeakNum (drop 1 xs) ys

findGenericSumTo :: [Int] -> Int -> [[Int]]
findGenericSumTo [] _ = []
findGenericSumTo xs n = filter ((== n) . sum) $ drop 2 $ inits xs ++ findGenericSumTo (drop 1 xs) n

hasSummands :: [Int] -> Int -> Bool
hasSummands nums n = find nums
  where hs = S.fromList nums
        find [] = False
        find (x:xs) = (S.member (n - x) hs && 2*x /= n) || find xs