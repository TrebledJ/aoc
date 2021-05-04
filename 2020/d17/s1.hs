import Control.Monad.State

type Grid = [[[Bool]]] -- It's the Matrix!
type Point = (Int, Int, Int) -- A 3D point.

runs = 6

main :: IO ()
main = readFile "input.txt" >>= print . evalState eval . execState (sequence $ replicate runs solve) . makeInitialState . lines

makeInitialState :: [[Char]] -> Grid
makeInitialState frame = outer ++ [middle] ++ outer
  where layer = replicate sz $ replicate sz False
        outer = replicate runs layer
        middle = middleOuter ++ map ((replicate runs False ++) . (++ replicate runs False) . map (== '#')) frame ++ middleOuter
        middleOuter = replicate runs (replicate sz False)
        sz = length frame + 2 * runs

at :: Grid -> Point -> Bool
g `at` (x, y, z) = ((g !! z) !! y) !! x

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

solve :: State Grid Int
solve = do
  grid <- get
  let layers = length grid
      size = length $ head grid
      neighbours (x, y, z) = [ (i, j, k) | i <- [x-1..x+1], j <- [y-1..y+1], k <- [z-1..z+1], 0 <= i, i < size, 0 <= j, j < size, 0 <= k, k < layers, i /= x || j /= y || k /= z ]
      countActiveNeighbours = countIf (== True) . map (grid `at`) . neighbours

  put $ foldr (\z acc ->
        foldr (\y acc ->
          foldr (\x acc ->
              let c = countActiveNeighbours (x, y, z)
                  res = if grid `at` (x, y, z)
                        then
                          (c == 2 || c == 3)
                        else
                          (c == 3)
              in res : acc
          ) [] [0..size-1] : acc
        ) [] [0..size-1] : acc
      ) [] [0..layers-1]

  return 0

eval :: State Grid Int
eval = do
  grid <- get
  return . sum . map (sum . map (countIf (== True))) $ grid
