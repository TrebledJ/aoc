import Data.List
import qualified Data.List.Split as Spl

main :: IO ()
main = readFile "input.txt" >>= print . solve . lines

solve :: [String] -> Int
solve [n, xs] = uncurry (*) $ minimum $ map (\x -> ((x - (x0 `mod` x)) `mod` x, x)) buses
  where x0 = read n     -- Earliest departure time.
        buses = map read $ filter (/= "x") $ Spl.splitOn "," xs
