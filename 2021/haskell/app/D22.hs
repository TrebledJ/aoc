-- See 2021/writeups/d22.md.
module D22 where

import           Data.List
import           Data.Maybe
import qualified Data.Vector                   as V
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Utils

type Cuboid = ((Int, Int), (Int, Int), (Int, Int))
type CuboidTag = Int -- Tag for indexing cuboids so instead of storing 6 ints for a cuboid, we just store 1.
type CuboidLU = V.Vector Cuboid -- Cuboid lookup.

type Command = (Bool, Cuboid)
type TagUnion = [CuboidTag]
type AlternatingUnions = [TagUnion] -- List of sets, alternating with + on even indices and - on odd indices.

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
  return (cmd == "on", ((x1, x2), (y1, y2), (z1, z2)))

part1 :: [Command] -> Int
part1 cmds = evalUnions lu (cuboidWithRadius 50) set
  where (lu, set) = mkExpr cmds

part2 :: [Command] -> Int
part2 cmds = evalUnions lu (cuboidWithRadius 200000) set
  where (lu, set) = mkExpr cmds

cuboidWithRadius :: Int -> Cuboid
cuboidWithRadius r = ((-r, r), (-r, r), (-r, r))

-- Constructs a sum-of-unions expression.
mkExpr :: [Command] -> (CuboidLU, AlternatingUnions)
mkExpr = foldl go (mempty, []) . zip [0 ..]
 where
  go (lu, cs) (la, (on, bounds)) =
    (lu <> pure bounds, map unionTerm cs ++ mExtra)
   where
    unionTerm xs = xs ++ [la]
    mExtra = if even (length cs) == on then [[la]] else [] -- A new union term is added if "on" and even, or "off" and odd.

evalUnions :: CuboidLU -> Cuboid -> AlternatingUnions -> Int
evalUnions lu scope =
  foldl (\acc (i, xs) -> if even i then acc + val xs else acc - val xs) 0
    . zip [0 ..]
 where
  val xs = evalUnion xs True scope
  -- Apply DFS to compute and sum intersections.
  evalUnion []  _   _   = 0 -- We've hit a leaf.
  evalUnion xs' add int = sum $ map explore $ init $ tails xs'
   where
    explore (x : xs'') = case intersection int (lu V.! x) of
      Just c ->
        let v = evalCube c
        in  (if add then v else negate v) + evalUnion xs'' (not add) c
      Nothing -> 0 -- Pruning. Bye bye branch.
    explore [] = undefined

evalCube :: Num a => ((a, a), (a, a), (a, a)) -> a
evalCube ((x1, x2), (y1, y2), (z1, z2)) =
  (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

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
