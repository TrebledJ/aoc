import Prelude
import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Char)
type Tile = Char
empty :: Char; empty = 'L'
occupied :: Char; occupied = '#'

width :: Grid -> Int
width = V.length . V.head

height :: Grid -> Int
height = length

tile :: Grid -> Int -> Int -> Tile
tile g r c
  | r < 0 || c < 0 || r >= height g || c >= width g     = error "Not a tile!"
  | otherwise                                           = (g V.! r) V.! c

(!) :: Grid -> (Int, Int) -> Tile
(!) = uncurry . tile

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

parse :: String -> Grid
parse = V.fromList . map V.fromList . lines

solve :: Grid -> Int
solve grid
  | grid == next          = sum $ V.map (vcount occupied) next
  | otherwise             = solve next
  where next = simulate grid

simulate :: Grid -> Grid
simulate g = V.map (V.map (step g)) $ V.generate (height g) $ V.generate (width g) . (,)

-- Computes the next output of the given coordinate on the grid.
step :: Grid -> (Int, Int) -> Tile
step g pos
  | t == empty && adj == 0      = occupied
  | t == occupied && adj >= 4   = empty
  | otherwise                   = t
  where adj = countAdj occupied g pos
        t = g ! pos

-- Counts the appearances of a tile around a cell.
countAdj :: Tile -> Grid -> (Int, Int) -> Int
countAdj t g (r, c) = count t [g ! (r+i, c+j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0, 
                                                0 <= r+i, r+i < height g,
                                                0 <= c+j, c+j < width g]

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

vcount :: (Eq a) => a -> V.Vector a -> Int
vcount x = length . V.filter (== x)
