import qualified Data.HashSet as HashSet


magicNumber = 2020


findNThatSumTo :: Int -> Int -> HashSet.HashSet Int -> Maybe [Int]
findNThatSumTo n sum hs = foldr go Nothing hs
  where go _ j@(Just _) = j
        go x Nothing
          | n == 2 =  if HashSet.member (sum - x) hs && 2*x /= sum
                      then Just [x, sum - x]
                      else Nothing
          | n > 2 = (x:) <$> findNThatSumTo (n - 1) (sum - x) (HashSet.delete x hs)


main :: IO ()
main = do
  set <- HashSet.fromList . map read . lines <$> readFile "input.txt"
  case findNThatSumTo 4 magicNumber set of
    Just xs -> do
      putStr "Numbers: "
      print xs
      putStr "Sum: "
      print $ sum xs
      putStr "Answer: "
      print $ product xs
    Nothing -> do
      print "Solution not found. :("
  
  return ()
