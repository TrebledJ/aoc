module D17 where


import           Data.List
-- import           Text.Megaparsec         hiding ( parse )
import           Text.Megaparsec.Char
import           Utils



main :: IO ()
main = defaultMainWithParser defaultFile parser part1 part2

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

part1 :: (Int, Int, Int, Int) -> Int
part1 (_, _, ymin, ymax) = gauss $ head $ initialYVelocities ymin ymax
  where gauss n = n * (n + 1) `div` 2

part2 :: (Int, Int, Int, Int) -> Int
part2 (xmin, xmax, ymin, ymax) = sum
  [ length $ initialXVelocities $ fromIntegral v0
  | v0 <- initialYVelocities ymin ymax
  ]
 where
  initialXVelocities v0 = nub
    [ u0
    | i  <- [ceiling (y_i_enter v0 ymax) .. floor (y_i_exit v0 ymin)]
    , u0 <- [xmax, xmax - 1 .. min_u0 - 1]
    , let x = floor $ x_i (fromIntegral u0) (fromIntegral i)
    , xmin <= x && x <= xmax
    ]
  x_i :: Float -> Float -> Float
  x_i u0 i = let ii = min i u0 in -0.5 * ii * ii + (u0 + 0.5) * ii
  min_u0 = ceiling $ -0.5 + sqrt (0.25 + 2 * fromIntegral xmin)

initialYVelocities :: Int -> Int -> [Int]
initialYVelocities ymin ymax = filter (hits . fromIntegral)
                                      [5 * yr, 5 * yr - 1 .. ymin]
 where
  hits v0 = ceiling (y_i_enter v0 ymax) <= floor (y_i_exit v0 ymin) -- Whether v0 will hit the target.
  yr = ymax - ymin

-- Math math math.
y_i_enter, y_i_exit :: Float -> Int -> Float
y_i_enter v0 ymax = (v0 + 0.5) + sqrt ((v0 + 0.5) ^^ 2 - 2 * fromIntegral ymax)
y_i_exit v0 ymin = (v0 + 0.5) + sqrt ((v0 + 0.5) ^^ 2 - 2 * fromIntegral ymin)

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
