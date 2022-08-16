-- Uses double counter method inspired from https://www.reddit.com/r/adventofcode/comments/rfzq6f/comment/hohc8vc/.
-- One counter keeps track of the number of occurrences of a character.
-- A second counter to keep track of pairs (and the number of times they occur) in the current string.
module Days.D14 where

import qualified Data.HashMap.Strict           as M
import           Data.List
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Utils


main :: IO ()
main = defaultMain defaultFile parser part1 part2

defaultFile :: String
defaultFile = "../input/d14.txt"

parser :: Parser (String, M.HashMap String Char)
parser =
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
  start'   = M.fromList $ zipWith (\a b -> ([a, b], 1)) start (tail start)
  (ctr, _) = iterate (uncurry (step rs)) (counter start, start') !! n
  sorted   = sort $ M.elems ctr
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
    newOccur0 = M.adjust (\x -> x - times) pr occur' -- Decrement existing pair.
    newOccur1 = M.insertWith (+) [a, rs M.! pr] times newOccur0 -- Increment new pairs.
    newOccur  = M.insertWith (+) [rs M.! pr, b] times newOccur1 -- Increment new pairs.
  update _ _ _ = undefined
