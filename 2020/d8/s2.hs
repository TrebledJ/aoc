{-# LANGUAGE TupleSections #-}

import Prelude
import qualified Data.HashSet as S
import qualified Data.Vector as V

type Operation = (String, Int)

main :: IO ()
main = readFile "input.txt" >>= print . solve . lines

-- Methodology:
--  * Generate a preprocessed vector containing True/False depending on a command generates an infinite loop.
--  * Then we'll iterate through the vector from start to finish.
--  * If the command is a nop, check if jmp would cause it to go out of the loop (and vice versa).

solve :: [String] -> Int
solve ls = computeAcc replaced 0 0
  where replaced = ops V.// [(index, (opToReplace, snd (ops V.! index)))]
        preprocessed = foldr (runLoop ops) (V.fromList $ replicate n False) [0..n-1]
        opToReplace = if fst (ops V.! index) == "jmp" then "nop" else "jmp"
        index = searchForReplacement ops preprocessed
        n = length ls
        ops = V.fromList $ map parse ls

computeAcc :: V.Vector Operation -> Int -> Int -> Int
computeAcc ops i acc 
  | i == V.length ops = acc
  | op == "acc" = next (acc + val)
  | otherwise = next acc
  where (op, val) = ops V.! i
        next = computeAcc ops (nextIndex ops i)

-- Returns the index which should be replaced.
searchForReplacement :: V.Vector Operation -> V.Vector Bool -> Int
searchForReplacement = searchForReplacementImpl 0

searchForReplacementImpl :: Int -> V.Vector Operation -> V.Vector Bool -> Int
searchForReplacementImpl i ops lps
  | op == "jmp" = if i+1 == n || notLoop (i+1)  -- Try changing it to nop and simulating.
                  then i                        -- Return the current index.
                  else next
  | op == "nop" = let i' = i + val in
                  if (0 <= i' && i' <= n)  && notLoop i'
                  then i
                  else next
  | otherwise = next
  where (op, val) = ops V.! i
        next = searchForReplacementImpl (nextIndex ops i) ops lps
        notLoop = not . (lps V.!)
        n = V.length ops
  
nextIndex :: V.Vector Operation -> Int -> Int
nextIndex ops i = if op == "jmp" then i + val else i + 1
  where (op, val) = ops V.! i

-- Check whether a command starting at the given index generates an infinite loop
-- and updates the corresponding cells. Bool indicates whether the ith command will loop.
-- Returns the updated vector.
runLoop :: V.Vector Operation -> Int -> V.Vector Bool -> V.Vector Bool
runLoop ops i lps
  | lps V.! i = lps   --  Base case: this is already a loop.
  | otherwise = runLoopImpl S.empty i ops lps

runLoopImpl :: S.HashSet Int -> Int -> V.Vector Operation -> V.Vector Bool -> V.Vector Bool
runLoopImpl visited i ops lps
  | i == length ops = lps   -- Return lps with no change.
  | S.member i visited || lps V.! i = assertAt lps (S.toList visited)  -- Assert visited ops, since they lead to this loop.
  | fst (ops V.! i) `elem` ["acc", "jmp", "nop"] = runLoopImpl (S.insert i visited) (nextIndex ops i) ops lps
  | otherwise = error $ "Unknown condition (runLoopImpl)"

-- Takes a vector<Bool>, and asserts the indices provided by a list of ints.
assertAt :: V.Vector Bool -> [Int] -> V.Vector Bool
assertAt v xs = v V.// map (,True) xs

-- Parsing.
parse :: String -> Operation
parse = fmap (parseNum . drop 1) . break (== ' ')

parseNum :: String -> Int
parseNum ('+':x) = read x
parseNum x = read x