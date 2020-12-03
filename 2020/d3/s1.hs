import Prelude

solve :: [String] -> Int
solve ss = length $ filter isTree [1..length ss - 1]
  where isTree i = (ss !! i !! ((3*i) `mod` length (head ss))) == '#'

main :: IO ()
main = do
  text <- lines <$> readFile "input.txt"
  print $ solve text
  return ()
