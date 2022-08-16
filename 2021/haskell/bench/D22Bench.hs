module D22Bench where

import qualified Criterion.Main as C
import qualified Days.D22 as D22
import Utils


main :: IO ()
main = criterionMain D22.defaultFile D22.parser $ \input ->
  [ C.bgroup "part1" [C.bench "part1" $ C.whnf D22.part1 input]
  , C.bgroup
    "part2"
    [ C.bench "sum of unions" $ C.whnf D22.part2 input
    , C.bench "sum of unions (optimised)" $ C.whnf D22.part2' input
    , C.bench "sum of unions (optimised 2)" $ C.whnf D22.part2'' input
    , C.bench "sum of intersections" $ C.whnf D22.part2i input
    ]
  ]