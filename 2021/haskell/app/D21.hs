-- Part 2 Approach.
-- 
-- Our state will be a `HashMap (Int, Int, Int, Int) Int`, which maps (p1, p2,
-- s1, s2) to the number of universes. On each step, for each pair ((p1, p2, s1,
-- s2), copies), generate a list of new pairs as if simulating those universes.
-- Combine all new pairs of ((p1', p2', s1', s2'), copies) into a new map.
module D21 where

import qualified Data.HashMap.Strict           as M
import           Data.List
import           Utils


main :: IO ()
main = defaultMain defaultFile parse part1 part2

defaultFile :: String
defaultFile = "../input/d21.txt"

parse :: String -> (Int, Int)
parse = (\[a, b] -> (a, b)) . map (read . last . words) . lines

part1 :: (Int, Int) -> Int
part1 start = sim start (0, 0) 0 1
 where
  sim (p1, p2) (s1, s2) d t
    | s1 >= 1000 = s2 * d
    | s2 >= 1000 = s1 * d
    | otherwise = if t == 1
      then sim (move p1 step, p2) (s1 + move p1 step, s2) d' 2
      else sim (p1, move p2 step) (s1, s2 + move p2 step) d' 1
   where
    (step, d') = roll 3 d
    move p st = (p + st - 1) `mod` 10 + 1
  roll n d = (sum [ (d + i) `mod` 100 | i <- [1 .. n] ], d + n)

part2 :: (Int, Int) -> Int
part2 (p1, p2) = step initState 1 (0, 0)
 where
  initState = M.singleton (p1, p2, 0, 0) 1
  step state t (w1, w2) | null state = max w1 w2
                        | otherwise  = step state' (1 - t) (w1 + w1', w2 + w2')
    where (state', w1', w2') = M.foldlWithKey' (go t) (M.empty, 0, 0) state

  -- Get state successors, partition into completed ones, and insert active universes into new map.
  go t (state, w1, w2) k cop = (state', w1 + sumCopies w1', w2 + sumCopies w2')
   where
    (active, done) =
      partition (\((_, _, s1, s2), _) -> s1 < 21 && s2 < 21) (succ t k cop)
    state'     = M.unionWith (+) state $ M.fromList active
    (w1', w2') = partition (\((_, _, s1, _), _) -> s1 >= 21) done
    sumCopies  = sum . map snd

  -- List successors of a pair. To simulate the dirac die, we multiple prev copies with new copies (c * copies).
  succ t (p1, p2, s1, s2) c =
    [ ((p1', p2', s1', s2'), c * copies)
    | (sum_, copies) <- sumToCopies
    , let (p1', p2') = if t == 1
            then ((p1 + sum_ - 1) `mod` 10 + 1, p2)
            else (p1, (p2 + sum_ - 1) `mod` 10 + 1)
    , let (s1', s2') = if t == 1 then (s1 + p1', s2) else (s1, s2 + p2')
    ]
  sumToCopies = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]
