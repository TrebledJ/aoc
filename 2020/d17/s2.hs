-- Uses vector for O(1) lookup.
import           Control.Monad.State
import qualified Data.Vector         as V
import Data.List

data Grid = Grid {
  values    :: V.Vector Bool,
  frameSize :: Int,
  outerSize :: Int
}
type Point = (Int, Int, Int, Int) -- A 3D point.

runs = 6

main :: IO ()
main = readFile "input.txt" >>= print . evalState eval . execState (sequence $ replicate runs solve) . makeInitialState . lines

layer_z = runs
layer_w = runs

instance Show Grid where
  show g = intercalate "\n" [ [ if g `at` (i, j, layer_z, layer_w) then '#' else '.' | i <- [0..sz-1] ] | j <- [0..sz-1] ]
    where sz = frameSize g

at :: Grid -> Point -> Bool
g `at` p = (values g) V.! (index4d g p)

index4d :: Grid -> Point -> Int
index4d g = index4dFromSizes (frameSize g) (outerSize g)

index4dFromSizes :: Int -> Int -> Point -> Int
index4dFromSizes f o (x, y, z, w) = x + y*f + z*f*f + w*o*f*f

indexToPoint :: Grid -> Int -> Point
indexToPoint g i = (x, y, w, z)
  where (w, w') = i `divMod` (o * f * f)
        (z, z') = w' `divMod` (f * f)
        (y, x) = z' `divMod` f
        f = frameSize g
        o = outerSize g

makeInitialState :: [[Char]] -> Grid
makeInitialState input = Grid { values=defaultGrid V.// pairs, frameSize=fsz, outerSize=osz }
  where defaultGrid = V.replicate (fsz * fsz * osz * osz) False
        f val (x, y) = (index4dFromSizes fsz osz (x, y, runs, runs), val == '#')
        pairs = zipWith f (concat input) [ (j, i) | i <- [runs..fsz-1-runs], j <- [runs..fsz-1-runs] ] -- Index-value pairs to update.
        fsz = length input + 2 * runs -- Frame size, expanded to account for #runs
        osz = 2 * runs + 1 -- Size of outer dimensions.

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

neighbours :: Grid -> Point -> [Point]
neighbours g (x, y, w, z) = [ (i, j, k, l) | i <- [x-1..x+1], j <- [y-1..y+1], k <- [z-1..z+1], l <- [w-1..w+1], 0 <= i, i < size, 0 <= j, j < size, 0 <= k, k < layers, 0 <= l, l < layers, i /= x || j /= y || k /= z || l /= w ]
  where size = frameSize g
        layers = outerSize g

countActiveNeighbours :: Grid -> Point -> Int
countActiveNeighbours g = countIf (== True) . map (g `at`) . neighbours g

solve :: State Grid Int
solve = do
  g <- get
  put $ Grid {
      values=(V.imap (\i active ->
          let c = countActiveNeighbours g (g `indexToPoint` i) 
          in if active  then c == 2 || c == 3
                        else c == 3
        ) $ values g),
      frameSize=frameSize g,
      outerSize=outerSize g
    }

  return 0

eval :: State Grid Int
eval = do
  grid <- get
  return . V.length . V.filter (== True) $ values grid
