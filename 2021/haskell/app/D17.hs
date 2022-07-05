-- Common abbreviations/terms used below:
--  u0: initial x velocity
--  v0: initial y velocity
--  i: the iteration/step of simulating the pod
--  hit: whether the pod successfully landed in the target zone
module D17 where


import           Data.List
import           Text.Megaparsec.Char
import           Utils


main :: IO ()
main = defaultMain defaultFile parser part1 part2

defaultFile :: String
defaultFile = "../input/d17.txt"

parser :: Parser (Int, Int, Int, Int)
parser =
  (,,,)
    <$  string "target area: x="
    <*> integer
    <*  string ".."
    <*> integer
    <*  string ", y="
    <*> integer
    <*  string ".."
    <*> integer

-- y = sum(v0 - j for j in 0..i-1) = -0.5i^2 + (v0 + 0.5)i            (1)

-- For a given v0 (initial y velocity), we can (in O(1) time) check if a pod
-- starting at v0 will reach a y-value within the target, i.e. ymin <= y <=
-- ymax. We calculate at what iteration the pod will enter/exit the target zone.
-- Note that the equation linking y-value and i is given by eq (1). Setting y =
-- ymin and y = ymax and solving the quadratic equation for both gives
-- `inverseStep` below. Although the quadratic equation normally gives two
-- solutions, we select the right-most solution, since we assume we're in the
-- falling stage of an upside-down parabola.
-- 
-- Having obtained the i (floating point) where y_i1 = ymin and y_i2 = ymax, we
-- determine if v0 can hit the target by checking if an integer lies between
-- ymin/ymax inclusive.
-- 
-- Finally, we perform a linear search for the highest v0 hitting the target and
-- apply Gauss' arithmetic formula to determine the max y-position.
part1 :: (Int, Int, Int, Int) -> Int
part1 (_, _, ymin, ymax) = gauss $ head $ initialV0 ymin ymax
  where gauss n = n * (n + 1) `div` 2

-- For each known v0, list the i that land in the target zone (i.e. ymin <= y_i
-- <= ymax). If an u0 exists such that xmin <= x_i <= xmax, add u0 to a set
-- associated with v0. You know the rest.
part2 :: (Int, Int, Int, Int) -> Int
part2 (xmin, xmax, ymin, ymax) = sum
  [ length $ initialU0 $ fromIntegral v0
  | v0 <- initialV0 ymin ymax
  ]
 where
  initialU0 v0 = nub
    [ u0
    | i  <- [ceiling (inverseStep v0 ymax) .. floor (inverseStep v0 ymin)]
    , u0 <- [xmax, xmax - 1 .. min_u0 - 1]
    , let x = floor $ x_i (fromIntegral u0) (fromIntegral i)
    , xmin <= x && x <= xmax
    ]
  x_i :: Float -> Float -> Float
  x_i u0 i = let ii = min i u0 in -0.5 * ii * ii + (u0 + 0.5) * ii
  min_u0 = ceiling $ -0.5 + sqrt (0.25 + 2 * fromIntegral xmin)

initialV0 :: Int -> Int -> [Int]
initialV0 ymin ymax = filter (hits . fromIntegral)
                                      [5 * yr, 5 * yr - 1 .. ymin] -- This range is kinda arbitrary.
 where
  hits v0 = ceiling (inverseStep v0 ymax) <= floor (inverseStep v0 ymin) -- Whether v0 will hit the target.
  yr = ymax - ymin

-- Math math math.
inverseStep :: Float -> Int -> Float
inverseStep v0 y = (v0 + 0.5) + sqrt ((v0 + 0.5) ^^ 2 - 2 * fromIntegral y)

-- Helper function for debugging.
simulate :: (Int, Int, Int, Int) -> Int -> Int -> Int
simulate (xmin, xmax, ymin, ymax) u0 v0 = simulate' 0 0 0 u0 v0
 where
  simulate' i x y u v
    | trace
      (  "iter: "
      ++ show i
      ++ ";\tpos: "
      ++ show x
      ++ ","
      ++ show y
      ++ ";\tvel: "
      ++ show u
      ++ ","
      ++ show v
      )
      False
    = undefined
  simulate' i x y u v
    | xmin <= x && x <= xmax && ymin <= y && y <= ymax = trace
      "success, reached target"
      1
    | x > xmax = trace "fail, overshot x" 0
    | y < ymin = trace "fail, overshot y" 0
    | otherwise = simulate' (i + 1) (x + u) (y + v) (u - sgn u) (v - 1)
  sgn x | x > 0     = 1
        | x < 0     = (-1)
        | otherwise = 0
