-- import qualified Data.HashMap.Strict as M

-- {- Approach:
--     * We'll use a hash map to store when a particular number was last seen.
--     * If the current number could not be found in the map, then it is new and 
--       the next number is 0.
--     * Otherwise the difference between the current turn and the stored turn is 
--       returned.
--  -}

-- input = [7,12,1,0,16,2]

-- data State = State { num :: Int --  The most recent number. The next state needs to consider this.
--                    , memory :: M.HashMap Int Int  --  A lookup table, mapping a number to the turn where it was last called.
--                    } deriving (Eq, Show)

-- nullState = State {num=0, memory=mempty}

-- main :: IO ()
-- main = print . solve $ input

-- -- solve :: [Int] -> Int
-- -- solve xs = num $ foldl solveIter (preprocess xs) [length xs..2019]
-- solve xs = foldl solveIter (preprocess xs) [length xs..2019]

-- preprocess :: [Int] -> State
-- preprocess xs = preprocess' xs nullState 0
--   where preprocess' [] s _ = s
--         preprocess' (x:xs) s i = preprocess' xs nextState (i+1)
--           where nextState = State {num=x, memory=M.insert x i $ memory s}

-- solveIter s i = s

-- -- solveIter :: State -> Int -> State
-- -- solveIter State{num=num, memory=memory} i = 
type Command = (Char, Int)

-- Consists of current coords, and direction (0-3, 0=NORTH, 1=EAST).
data State = State { x :: Int, y :: Int, dir :: Int } deriving (Eq, Show)

initState :: State
initState = State 0 0 1

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [Command]
parse = map parsel . lines
  where parsel (a:n) = (a, read n)

solve :: [Command] -> Int
solve = res . foldl solveFold initState
  where res State{x=x', y=y'} = abs x' + abs y'

solveFold :: State -> Command -> State
solveFold State{x=x', y=y', dir=dir'} (action, v)
  | action == 'N'       = State {x=x',      y=y' - v,   dir=dir'}
  | action == 'E'       = State {x=x' + v,  y=y',       dir=dir'}
  | action == 'S'       = State {x=x',      y=y' + v,   dir=dir'}
  | action == 'W'       = State {x=x' - v,  y=y',       dir=dir'}
  | action == 'L'       = State {x=x',      y=y',       dir=(dir' - v `div` 90) `mod` 4}
  | action == 'R'       = State {x=x',      y=y',       dir=(dir' + v `div` 90) `mod` 4}
  | action == 'F'       = State {x=x' + v*fwdx,
                                 y=y' + v*fwdy,
                                 dir=dir'}
  where offsets = [(0, -1), (1, 0), (0, 1), (-1, 0)]
        (fwdx, fwdy) = offsets !! dir'
