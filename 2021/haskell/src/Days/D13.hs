-- CAFJHZCK
module Days.D13 where

import qualified Data.HashSet                  as S
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Utils


type Coor = (Int, Int)
data Axis = X | Y deriving (Read, Show, Eq)
type Fold = (Axis, Int)

parse :: Parser ([Coor], [Fold])
parse =
  (,) <$> some (coor <* newline) <* newline <*> folds `sepBy1` newline <* eof
 where
  coor = (,) <$> digits <* char ',' <*> digits
  folds =
    (,) <$ string "fold along " <*> (axis <$> letterChar) <* char '=' <*> digits
  axis 'x' = X
  axis _   = Y

part1 :: ([Coor], [Fold]) -> Int
part1 (cs, f : _) = foldPaper (S.fromList cs) f |> length
part1 (_ , _    ) = undefined

part2 :: ([Coor], [Fold]) -> String
part2 (cs, fs) = '\n' : grid
 where
  dots = fs |> foldl foldPaper (S.fromList cs)
  grid = makeGrid dots |> unlines

foldPaper :: S.HashSet Coor -> Fold -> S.HashSet Coor
foldPaper cs (ax, v) = cs |> S.map mapCoor
 where
  mapCoor (x, y)
    | ax == X   = if x >= v then (x - 2 * abs (x - v), y) else (x, y)
    | otherwise = if y >= v then (x, y - 2 * abs (y - v)) else (x, y)

makeGrid :: S.HashSet Coor -> [String]
makeGrid cs =
  [ [ if (i, j) `elem` cs then '#' else '.' | i <- [w1 .. w2] ]
  | j <- [h1 .. h2]
  ]
 where
  w1 = cs |> S.map fst |> minimum
  w2 = cs |> S.map fst |> maximum
  h1 = cs |> S.map snd |> minimum
  h2 = cs |> S.map snd |> maximum
