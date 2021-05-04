-- Uses vector for O(1) lookup.
import Control.Monad.State
import qualified Data.Vector as V

type Grid = V.Vector (V.Vector (V.Vector (V.Vector Bool)))
type Point = (Int, Int, Int, Int) -- A 3D point.

runs = 6

main :: IO ()
main = readFile "input.txt" >>= print . evalState eval . execState (sequence $ replicate runs solve) . makeInitialState . lines

makeInitialState :: [[Char]] -> Grid
makeInitialState frame = outer2 V.++ V.singleton middle2 V.++ outer2
  where layer = V.replicate sz $ V.replicate sz False
        outer = V.replicate (2 * runs + 1) layer
        outer2 = V.replicate runs outer
        middle = middleOuter V.++ V.map ((layerSide V.++) . (V.++ layerSide) . V.map (== '#')) vframe V.++ middleOuter
        layerSide = V.replicate runs False
        middleOuter = V.replicate runs (V.replicate sz False)
        middle2 = V.replicate runs layer V.++ V.singleton middle V.++ V.replicate runs layer 
        vframe = V.fromList $ map V.fromList frame
        sz = length frame + 2 * runs

at :: Grid -> Point -> Bool
g `at` (x, y, z, w) = (((g V.! w) V.! z) V.! y) V.! x

countIf :: (a -> Bool) -> V.Vector a -> Int
countIf p = V.length . V.filter p

solve :: State Grid Int
solve = do
  grid <- get
  let layers = V.length grid
      size = V.length $ V.head $ V.head grid
      neighbours (x, y, z, w) = V.fromList [ (i, j, k, l) | i <- [x-1..x+1], j <- [y-1..y+1], k <- [z-1..z+1], l <- [w-1..w+1], 0 <= i, i < size, 0 <= j, j < size, 0 <= k, k < layers, 0 <= l, l < layers, i /= x || j /= y || k /= z || l /= w ]
      countActiveNeighbours = countIf (== True) . V.map (grid `at`) . neighbours

  put $ V.imap (\w v3 ->
      V.imap (\z v2 ->
        V.imap (\y v ->
          V.imap (\x val ->
              let c = countActiveNeighbours (x, y, z, w)
                  res = if val
                        then (c == 2 || c == 3)
                        else (c == 3)
              in res
          ) v
        ) v2
      ) v3
    ) grid

  return 0

eval :: State Grid Int
eval = do
  grid <- get
  return . V.sum . V.map (V.sum . V.map (V.sum . V.map (countIf (== True)))) $ grid
