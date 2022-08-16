module D22Bench where

import qualified Criterion.Main                as C
import qualified Days.D22                      as D22
import           Utils


main :: IO ()
main = criterionMain (getDefaultFile 22) D22.parse $ \input ->
  [ C.bgroup "part1" [C.bench "part1" $ C.whnf D22.part1 input]
  , C.bgroup
    "part2"
    [ C.bench "sum of unions" $ C.whnf D22.part2u input
    , C.bench "sum of unions (optimised)" $ C.whnf D22.part2u' input
    , C.bench "sum of unions (optimised 2)" $ C.whnf D22.part2u'' input
    , C.bench "sum of intersections" $ C.whnf D22.part2i input
    ]
  ]
