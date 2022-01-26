module D04 where

import           Data.List
import           Data.List.Split
import           Utils


main :: IO ()
main = defaultMain defaultFile parse part1 part2

defaultFile :: String
defaultFile = "../input/d04.txt"

-- A bingo grid, vectorised.
type Grid = [Int]

at :: Grid -> Int -> Int -> Int
at g x y = g !! (y * 5 + x)

wins :: [Int] -> Grid -> Bool
wins hist g = or [ hit [ at g i j | j <- [0 .. 4] ] | i <- [0 .. 4] ]
    || or [ hit [ at g i j | i <- [0 .. 4] ] | j <- [0 .. 4] ]
    -- || hit [at g i i | i <- [0..4]] -- Diagonals don't count?
    -- || hit [at g i (4-i) | i <- [0..4]]
    where hit = all (`elem` hist)

score :: [Int] -> Grid -> Int
score hist g = sum [ x | x <- g, x `notElem` hist ] * last hist

parse :: String -> ([Int], [Grid])
parse s =
    let (xs : _ : rest) = lines s
        grids = map (map read . concatMap words) $ splitOn [""] rest
    in  (map read $ splitOn "," xs, grids)

-- Iterates over the numbers, returns a list of pairs: (iter i, grids that completed on iter i).
iter :: [Int] -> [Grid] -> [(Int, [Grid])]
iter ns gs = iter' ns gs 1
  where
    iter' ns' gs' i | i == length ns' = []
                    | otherwise       = (i, won) : iter' ns' ongoing (i + 1)
        where (won, ongoing) = partition (wins (take i ns')) gs'

part1 :: ([Int], [Grid]) -> Int
part1 (ns, gs) = score (take i ns) (head firstWinGroup)
    where (i, firstWinGroup) = firstBy (not . null . snd) $ iter ns gs

part2 :: ([Int], [Grid]) -> Int
part2 (ns, gs) = score (take i ns) (head firstWinGroup)
    where (i, firstWinGroup) = lastBy (not . null . snd) $ iter ns gs
