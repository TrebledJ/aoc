import qualified Data.HashSet as S

magicNumber = 2020

main :: IO ()
main = readFile "input.txt" >>= print . solve

solve :: String -> Int
solve = uncurry (*) . findNumbers . map read . lines

findNumbers :: [Int] -> (Int, Int)
findNumbers nums = find nums
  where hs = S.fromList nums
        find [] = (0, 0)
        find (x:xs) = if S.member (magicNumber - x) hs && 2*x /= magicNumber
                      then (x, magicNumber - x)
                      else find xs
