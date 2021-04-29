import Text.Parsec
import Utils

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse notes ""

solve :: Either ParseError Notes -> Int
solve (Left err) = error $ "error: " ++ show err
solve (Right (Notes fs _ ts)) = sum $ map check $ ts
  where -- Sums the error values.
        check = sum . filter (\t -> not $ any (valid t) fs)
