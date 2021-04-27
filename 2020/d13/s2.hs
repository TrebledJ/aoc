-- 7,13,x,x,59,x,31,19
-- t % 7 == 0
-- t % 13 == -1
-- t % 59 == -4
-- t % 31 == -6
-- t % 19 == -7

import Data.List
import qualified Data.List.Split as Spl

main :: IO ()
main = readFile "input.txt" >>= print . solve . Spl.splitOn "," . last . lines

solve :: [String] -> Integer
solve xs = (+1) $ fst $ crt $ map (fmap read) $ filter ((/= "x") . snd) $ zip [-1, -2..(-1000)] xs

-- Chinese Remainder Theorem.
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
  where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm.
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
      where (g, s, t) = gcd (b `mod` a) a

