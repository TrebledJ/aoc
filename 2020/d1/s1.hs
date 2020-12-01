import qualified Data.HashSet as HashSet

magicNumber = 2020


solve :: HashSet.HashSet Int -> Maybe (Int, Int)
solve hs = foldr go Nothing hs
  where go _ (Just n) = Just n
        go x Nothing =  if HashSet.member (magicNumber - x) hs && 2*x /= magicNumber
                        then Just (x, magicNumber - x)
                        else Nothing


main :: IO ()
main = do
  set <- HashSet.fromList . map read . lines <$> readFile "input.txt"
  case solve set of
    Just (a, b) -> do
      putStrLn $ "Numbers: " ++ show a ++ " " ++ show b
      putStr "Sum: "
      print $ a + b
      putStr "Answer: "
      print $ a * b
    Nothing -> do
      print "Solution not found. :("
  
  return ()
