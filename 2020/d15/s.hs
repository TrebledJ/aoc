{- Approach:
    * We'll use a hash map to store when a particular number was last seen.
    * If the current number could not be found in the map, then it is new and 
      the next number is 0.
    * Otherwise the difference between the current turn and the stored turn is 
      returned.
 -}

import qualified Data.HashMap.Strict as M

input = [7,12,1,0,16,2]
query = 2020  -- s1
-- query = 30000000 -- s2

data State = State { num :: Int --  The most recent number. The next state needs to consider this.
                   , memory :: M.HashMap Int Int  --  A lookup table, mapping a number to the turn where it was last called.
                   } deriving (Eq, Show)

nullState :: State
nullState = State {num=0, memory=mempty}

main :: IO ()
main = print . solve $ input

solve :: [Int] -> Int
solve xs = num $ foldl solveIter (preprocess xs) [length xs .. query-1]

preprocess :: [Int] -> State
preprocess nums = preprocess' nums nullState 1
  where preprocess' [x] s _ = State {num=x, memory=memory s}  -- Save number but don't save into memory.
        preprocess' (x:xs) s i = preprocess' xs nextState (i+1)
          where nextState = State {num=x, memory=M.insert x i $ memory s}

solveIter :: State -> Int -> State
solveIter State{num=n, memory=mem} turn
  | M.member n mem      = State {num=turn - (mem M.! n), memory=newMem} -- n is owold guy.
  | otherwise           = State {num=0, memory=newMem} -- n is a neuwu guy.
  where newMem = M.insert n turn mem
