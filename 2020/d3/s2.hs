import Prelude

slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

solve :: [String] -> (Int, Int) -> Int
solve ss (dx, dy) = length $ filter isTree [1..1+((h - 2) `div` dy)]
  where isTree i = (ss !! (dy*i) !! ((dx*i) `mod` w)) == '#'
        w = length $ head ss
        h = length ss

main :: IO ()
main = do
  text <- lines <$> readFile "input.txt"
  print $ product $ map (solve text) slopes
  return ()
