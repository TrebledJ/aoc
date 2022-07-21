-- See 2021/writeups/d22.md.
module D22 where

import qualified Criterion.Main                as C
import           Data.Bifunctor
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
type TagUnion = [CuboidTag] -- Union of tags.
type AlternatingUnions = [TagUnion] -- Alternating sum of unions: + on even indices, - on odd indices.

main :: IO ()
main = defaultMain defaultFile parser part1 part2
-- main = criterionMain defaultFile parser $ \input ->
--   [ C.bgroup "part1" [C.bench "part1" $ C.whnf part1 input]
--   , C.bgroup
--     "part2"
--     [ C.bench "sum of unions" $ C.whnf part2 input
--     , C.bench "sum of intersections" $ C.whnf part2i input
--     ]
--   ]

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
  go (lu, cs) (tag, (on, cuboid)) =
    (lu <> pure cuboid, map (tag :) cs ++ extra)
    where extra = if even (length cs) == on then [[tag]] else [] -- A new union term is added if "on" and even, or "off" and odd.

evalUnions :: CuboidLU -> Cuboid -> AlternatingUnions -> Int
evalUnions lu scope = sum
  . zipWith (\i ts -> (-1) ^ i * evalUnion True scope ts) [0 ..]
 where
  -- Apply DFS to compute and sum intersections.
  evalUnion _   _   [] = 0 -- We've hit a leaf.
  evalUnion add int ts = sum $ map explore $ init $ tails ts
   where
    explore (tag : ts') = case intersection int (lu V.! tag) of
      Just c ->
        let v = evalCuboid c
        in  (if add then v else negate v) + evalUnion (not add) c ts'
      Nothing -> 0 -- Pruning. Bye bye branch.
    explore [] = undefined

evalCuboid :: Cuboid -> Int
evalCuboid ((x1, x2), (y1, y2), (z1, z2)) =
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
rangeIntersection (a1, a2) (b1, b2) | a2 < b1 || b2 < a1   = Nothing
                                    | a1 <= b1 && b2 <= a2 = Just (b1, b2)
                                    | b1 <= a1 && a2 <= b2 = Just (a1, a2)
                                    | a1 <= b1 && a2 <= b2 = Just (b1, a2)
                                    | b1 <= a1 && b2 <= a2 = Just (a1, b2)
                                    | otherwise            = undefined


type ITerm = (Bool,   -- Whether to add (True) or subtract (False) this term.
              Cuboid) -- The cuboid belonging to the term.

part2i :: [Command] -> Int
part2i = evalExpri . mkExpri

mkExpri :: [Command] -> [ITerm]
mkExpri = foldl go []
 where
  go terms (on, cuboid) =
    terms ++ rest ++ (if on then [(True, cuboid)] else [])
   where
    rest = map (second fromJust) $ filter (isJust . snd) $ map
      (bimap not (intersection cuboid))
      terms

evalExpri :: [ITerm] -> Int
evalExpri terms =
  sum $ map (\(b, c) -> (if b then id else negate) $ evalCuboid c) terms