import Prelude
import qualified Data.HashSet as S
import qualified Data.Vector as V   -- For O(1) lookup.

main :: IO ()
main = readFile "input.txt" >>= print . solve . lines

solve :: [String] -> Int
solve = findLoop . V.fromList . map parse

parse :: String -> (String, Int)
parse = fmap (parseNum . drop 1) . break (== ' ')

parseNum :: String -> Int
parseNum ('+':x) = read x
parseNum x = read x

-- Finds the infinite loop.
findLoop :: V.Vector (String, Int) -> Int
findLoop = findImpl S.empty 0 0

findImpl :: S.HashSet Int -> Int -> Int -> V.Vector (String, Int) -> Int
findImpl visited acc i xs
  | S.member i visited = acc  --  Base case: we've found a loop, return the acc.
  | op == "acc" = findImpl nVisited (acc + val) (i + 1) xs
  | op == "jmp" = findImpl nVisited acc (i + val) xs
  | otherwise = findImpl nVisited acc (i + 1) xs
  where (op, val) = xs V.! i
        nVisited = S.insert i visited
