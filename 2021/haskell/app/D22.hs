module D22 where

import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Vector as V
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Utils

type Cuboid = ((Int, Int), (Int, Int), (Int, Int))
type CuboidL = Int -- Cuboid label to index cuboids.
type CuboidLU = V.Vector Cuboid -- Cuboid lookup.

data Command = Command
  { on    :: Bool
  , bound :: Cuboid
  }
  deriving Show
type UnionList a = [a]
type CountingSet a = [UnionList a] -- List of sets, alternating + (even index) and - (odd index).

maxCuboid :: Cuboid
maxCuboid = ((-200000, 200000), (-200000, 200000), (-200000, 200000))


main :: IO ()
main = defaultMain defaultFile parser part1 part2

defaultFile :: String
defaultFile = "../input/d22.txt"

parser :: Parser [Command]
parser = flip sepBy1 newline $ do
  cmd <- some letterChar
  string " x="
  x1 <- integer
  string ".."
  x2 <- integer
  string ",y="
  y1 <- integer
  string ".."
  y2 <- integer
  string ",z="
  z1 <- integer
  string ".."
  z2 <- integer
  return $ Command (cmd == "on") ((x1, x2), (y1, y2), (z1, z2))

part1 :: [Command] -> Int
part1 _ = 0 -- Implemented in Rust.

part2 :: [Command] -> Int
part2 cmds = countSet lu set where (lu, set) = mkExpr cmds

mkExpr :: [Command] -> (CuboidLU, CountingSet CuboidL)
mkExpr = foldl go (mempty, []) . zip [0 ..]
 where
  go (lu, cs) (la, Command { on = o, bound = b }) =
    (lu V.++ V.fromList [b], map unionTerm cs ++ mExtra)
   where
    unionTerm xs = xs ++ [la]
    mExtra = if o == even (length cs) then [[la]] else [] -- A new union term is added if "on" and even, or "off" and odd.

intersection :: Cuboid -> Cuboid -> Maybe Cuboid
intersection (ax, ay, az) (bx, by, bz)
  | any isNothing [xint, yint, zint]
  = Nothing
  | otherwise
  = let Just x = xint
        Just y = yint
        Just z = zint
    in  Just (x, y, z)
 where
  xint = rangeIntersection ax bx
  yint = rangeIntersection ay by
  zint = rangeIntersection az bz

rangeIntersection :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
rangeIntersection (a1, a2) (b1, b2) | a1 <= b1 && b2 <= a2 = Just (b1, b2)
                                    | b1 <= a1 && a2 <= b2 = Just (a1, a2)
                                    | a2 < b1 || b2 < a1   = Nothing
                                    | a1 <= b1 && a2 <= b2 = Just (b1, a2)
                                    | b1 <= a1 && b2 <= a2 = Just (a1, b2)
                                    | otherwise            = undefined

-- |A u B|
-- on:  |A u B u C|
-- off: |A u B| - |(A u B) n C|
--      |A u B| - |(A u B) n C| + |C| - |C| = |A u B u C| - |C|
-- off,on:  |A u B u C| - |C| + |D| - |(A u B u C) n D| + |C n D|
--          |A u B u C u D| - |C| + |C n D|
--          |A u B u C u D| - |C u D| + |D|
-- off,on,on:   |A u B u C u D| - |C u D| + |D| + |E| - |(A u B u C u D) n E| + |(C u D) n E| - |D n E|
--              |A u B u C u D u E| - |C u D u E| + |D u E|
-- off,on,on,on:  |A u B u C u D u E| - |C u D u E| + |D u E| + |F| - |(A u B u C u D u E) n F| + |(C u D u E) n F| - |(D u E) n F|
--                |A u B u C u D u E u F| - |C u D u E u F| + |D u E u F|
-- off,on,off:  |A u B u C u D| - |C u D| + |D| - |(A u B u C u D) n E| + |(C u D) n E| - |D n E|
--              |A u B u C u D u E| - |C u D u E| + |D| - |D n E|
--              |A u B u C u D u E| - |C u D u E| + |D u E| - |E|
-- off,off: |A u B u C| - |C| - |(A u B u C) n D| + |C n D|
--          |A u B u C u D| - |C| - |D| + |C n D|
--          |A u B u C u D| - |C u D|
-- off,off,on:  |A u B u C u D| - |C u D| + |E| - |(A u B u C u D) n E| + |(C u D) n E|
--              |A u B u C u D u E| - |C u D| + |(C u D) n E|
--              |A u B u C u D u E| - |C u D u E| + |E|
-- off,off,off: |A u B u C u D| - |C u D| - |(A u B u C u D) n E| + |(C u D) n E|
--              |A u B u C u D u E| - |C u D u E|
-- 

-- |X u Y| = |X| + |Y| - |X n Y|

{-
+ A
  - A n B
    + A n B n C
  - A n C
+ B
  - B n C
+ C

def f(A, n, d, set):
  for (i=d; i < n; i++)
    set += A[i]
    f(A, n, d+1)
    set -= A[i]
-}

get :: CuboidLU -> CuboidL -> Cuboid
get = (V.!)

countSet :: CuboidLU -> CountingSet CuboidL -> Int
countSet lu =
  foldl (\acc (i, xs) -> if even i then acc + val xs else acc - val xs) 0
    . zip [0 ..]
 where
  val xs | trace ("val: evaluating" ++$ xs) False = undefined
  val xs = evalUnion xs True maxCuboid
  -- evalUnion xs _ int | trace ("evalUnion: evaluating" ++$ xs ++$ int) False = undefined
  evalUnion []  _   _   = 0
  evalUnion xs' add int = sum $ map explore $ init $ tails xs'
   where
    explore (x : xs'') = case intersection int (lu `get` x) of
      Just c ->
        let v = evalCube c
        in  (if add then v else negate v) + evalUnion xs'' (not add) c
      Nothing -> 0 -- Short circuit out of this DFS branch.
    explore [] = undefined

evalCube :: Num a => ((a, a), (a, a), (a, a)) -> a
evalCube ((x1, x2), (y1, y2), (z1, z2)) =
  (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f (x : xs) = foldM f x xs
foldM1 _ []       = undefined
