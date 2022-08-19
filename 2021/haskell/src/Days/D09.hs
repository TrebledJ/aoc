-- BFS implementation.

module Days.D09 where

import           Data.Char
import qualified Data.HashSet                  as S
import           Data.List
import           Utils


type Cell = Char
type Grid = [[Cell]]
type Coor = (Int, Int)

parse :: String -> Grid
parse = lines

part1 :: Grid -> Int
part1 g = [ (i, j) | i <- [0 .. w - 1], j <- [0 .. h - 1] ] 
            |> filter isLowPoint 
            |> map (gridAt g)
            |> riskLevel
 where
  isLowPoint pos = pos |> gridAdjs g |> all (gridAt g pos <)
  (w, h) = g |> getWH

part2 :: Grid -> Int
part2 g = basins [] [ (i, j) | i <- [0 .. w - 1], j <- [0 .. h - 1] ] 
            |> map length 
            |> sort 
            |> reverse 
            |> take 3 
            |> product
 where
  basins v []        = v
  basins v (cur : q) = if (cur `elem`) `any` v
    then basins v q
    else basins (bfs g cur basinAdjs : v) q
  basinAdjs pos | gridAt g pos == '9' = []
                | otherwise = pos |> adjsG g |> filter (gridAt g .> (< '9'))
  (w, h) = getWH g

adjs :: Coor -> (Int, Int) -> [Coor]
adjs (x, y) (w, h) = filter isInside
                            [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
  where isInside (x', y') = 0 <= x' && x' < w && 0 <= y' && y' < h

adjsG :: Grid -> Coor -> [Coor]
adjsG g p = adjs p (getWH g)

getWH :: Grid -> Coor
getWH g = (length (head g), length g)

gridAdjs :: Grid -> Coor -> [Cell]
gridAdjs g pos = [ gridAt g adj | adj <- adjs pos (getWH g) ]

gridAt :: Grid -> Coor -> Cell
gridAt g (x, y) = (g !! y) !! x

riskLevel :: [Char] -> Int
riskLevel = map (digitToInt .> (+ 1)) .> sum

-- Returns all coordinates connected to given coordinate.
bfs
  :: Grid
  -> Coor -- First coordinate.
  -> (Coor -> [Coor]) -- A "neighbour-finding" function.
  -> S.HashSet Coor -- Returns connected coordinates.
bfs g coor = bfsImpl g S.empty [coor]

bfsImpl
  :: Grid
  -> S.HashSet Coor -- Visited coordinates.
  -> [Coor] -- Queued coordinates.
  -> (Coor -> [Coor]) -- A "neighbour-finding" function.
  -> S.HashSet Coor -- Returns connected coordinates.
bfsImpl _ v []        _ = v
bfsImpl g v (cur : q) f = if cur `elem` v
  then bfsImpl g v q f
  else bfsImpl g (cur `S.insert` v) (q ++ new) f
  where new = f cur |> filter (`notElem` v)
