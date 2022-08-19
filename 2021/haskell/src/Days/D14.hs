-- Uses double counter method inspired from https://www.reddit.com/r/adventofcode/comments/rfzq6f/comment/hohc8vc/.
-- One counter keeps track of the number of occurrences of a character.
-- A second counter to keep track of pairs (and the number of times they occur) in the current string.
module Days.D14 where

import qualified Data.HashMap.Strict           as M
import           Data.List
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Utils


parse :: Parser (String, M.HashMap String Char)
parse =
  (,)
    <$> some letterChar
    <*  space
    <*> (M.fromList <$> rule `sepBy1` newline)
    <*  eof
  where rule = (,) <$> some letterChar <* string " -> " <*> letterChar

part1 :: (String, M.HashMap String Char) -> Int
part1 = solve 10

part2 :: (String, M.HashMap String Char) -> Int
part2 = solve 40

solve :: Int -> (String, M.HashMap String Char) -> Int
solve n (start, rs) = most - least
 where
  start'   = start |> zipWith (\a b -> ([a, b], 1)) (tail start) |> M.fromList
  (ctr, _) = iterate (uncurry (step rs)) (counter start, start') !! n
  sorted   = ctr |> M.elems |> sort
  most     = last sorted
  least    = head sorted

step
  :: M.HashMap String Char -- Rules from input.
  -> M.HashMap Char Int -- Counter of existing chars.
  -> M.HashMap String Int -- Counter of pairs that previously occurred.
  -> (M.HashMap Char Int, M.HashMap String Int) -- Returns new counters of existing chars and pairs.
step rs ctr occur = M.foldlWithKey' update (ctr, occur) occur
 where
  update (ctr', occur') pr@[a, b] times = if pr `M.member` rs
    then (M.insertWith (+) (rs M.! pr) times ctr', newOccur)
    else (ctr', occur')
   where
    newOccur = occur' |> M.adjust (\x -> x - times) pr -- Decrement existing pair.
                      |> M.insertWith (+) [a, rs M.! pr] times  -- Increment new pairs.
                      |> M.insertWith (+) [rs M.! pr, b] times -- Increment new pairs.
  update _ _ _ = undefined
