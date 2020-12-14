import Prelude
import qualified Data.HashSet as S

window = 25

main :: IO ()
main = readFile "input.txt" >>= print . solve . map read . lines

solve :: [Int] -> Int
solve xs = findWeakNum xs (drop window xs)

findWeakNum :: [Int] -> [Int] -> Int
findWeakNum _ [] = error "Could not find the weak number."
findWeakNum xs (y:ys) = if not $ hasSummands (take window xs) y 
                        then y
                        else findWeakNum (drop 1 xs) ys

hasSummands :: [Int] -> Int -> Bool
hasSummands nums sum = find nums
  where hs = S.fromList nums
        find [] = False
        find (x:xs) = (S.member (sum - x) hs && 2*x /= sum) || find xs