module D22 where

import Control.Monad
import           Data.Maybe
-- import Data.Vector
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Utils

type Cuboid = ((Int, Int), (Int, Int), (Int, Int))

data Command = Command
  { on    :: Bool
  , bound :: Cuboid
  }
  deriving Show
-- data Cuboid = Cuboid Bound3
-- data CuboidSet = Value Cuboid | Sub Cuboid [Cuboid]
data CuboidExpr = ENull | ECuboid Cuboid | EAdd CuboidExpr CuboidExpr | ESub CuboidExpr CuboidExpr | EInter [CuboidExpr] deriving (Eq, Show)




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

part1 :: a -> Int
part1 x = 0

part2 :: [Command] -> Int
part2 cmds = traceShow expr $ eval expr
  where expr = mkExpr cmds 


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
                                    | a1 <= b1 && a2 <= b2 = Just (b1, a2)
                                    | b1 <= a1 && b2 <= a2 = Just (a1, b2)
                                    | otherwise            = Nothing

infixl 7 |&|
infixl 5 |+|, |-|
(|+|), (|-|), (|&|) :: CuboidExpr -> CuboidExpr -> CuboidExpr
(|+|) = EAdd
(|-|) = ESub
(|&|) (EAdd x y) c@(ECuboid _ ) = (x |&| c) |+| (y |&| c)
(|&|) (ESub x y) c@(ECuboid _ ) = (x |&| c) |-| (y |&| c)
(|&|) (EInter xs) (  EInter  ys) = EInter (xs ++ ys)
(|&|) (EInter xs) y              = EInter (xs ++ [y])
(|&|) x           (EInter ys)    = EInter (x : ys)
(|&|) (ECuboid c) (ECuboid d) | isNothing (intersection c d) = ENull -- Smol optimisation.
(|&|) x y                        = EInter [x, y]

mkExpr :: [Command] -> CuboidExpr
mkExpr cmds = fst $ foldl go (ENull, 0) cmds
 where
  cubes = map bound cmds
  -- go expr Command { on = o, bound = b } = (\x -> traceShow (eval x) x) $ trim $ if o
  --   then expr |+| cub |-| inter
  --   else expr |-| inter
  --  where
  --   cub   = ECuboid b
  --   inter = expr |&| cub
  go (expr, lit) Command { on = o, bound = b } = if o
    then (trim $ expr |+| cub |-| inter, trace' $ lit + eval cub - eval inter)
    else (trim $ expr |-| inter, trace' $ lit - eval inter)
   where
    cub   = ECuboid b
    inter = expr |&| cub

trim :: CuboidExpr -> CuboidExpr
trim (EAdd x y) = case trim x of -- TODO: simplify this ugliness?
  ENull -> trim y
  x'    -> case trim y of
    ENull -> x'
    y'    -> EAdd x' y'
trim (ESub x y) = case trim x of
  ENull -> trim y
  x'    -> case trim y of
    ENull -> x'
    y'    -> ESub x' y'
trim (EInter xs) | ENull `elem` xs = ENull
                 | otherwise       = EInter (map trim xs)
trim x = x

eval :: CuboidExpr -> Int
eval (ECuboid ((x1, x2), (y1, y2), (z1, z2))) = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)
eval (EAdd x y) = eval x + eval y
eval (ESub x y) = eval x - eval y
eval (EInter xs) = maybe 0 (eval . ECuboid) $ foldM1 intersection $ map toCuboid xs
  where toCuboid (ECuboid x) = x
        toCuboid _ = undefined
eval ENull = 0

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f (x:xs) = foldM f x xs
foldM1 _ _ = undefined
