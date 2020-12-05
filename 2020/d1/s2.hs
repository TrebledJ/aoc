import qualified Data.HashSet as S

magicNumber = 2020

main :: IO ()
main = readFile "input.txt" >>= print . solve

solve :: String -> Int
solve = product . findNThatSumTo 3 magicNumber . S.fromList . map read . lines

findNThatSumTo :: Int -> Int -> S.HashSet Int -> [Int]
findNThatSumTo n sum hs = find $ S.toList hs
  where find [] = []
        find (x:xs)
          | n == 2  = if S.member (sum - x) hs && 2*x /= sum
                      then [x, sum - x]
                      else find xs
          | n > 2   = if length sub == n - 1 
                      then x:sub 
                      else find xs
          where sub = findNThatSumTo (n - 1) (sum - x) (S.delete x hs)
