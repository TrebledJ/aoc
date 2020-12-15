type Command = (Char, Int)

-- Consists of current coords, and direction (0-3, 0=NORTH, 1=EAST).
data State = State { x :: Int, y :: Int, wx :: Int, wy :: Int } deriving (Eq, Show)

initState :: State
initState = State {x=0, y=0, wx=10, wy= -1}

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [Command]
parse = map parsel . lines
  where parsel (a:n) = (a, read n)

solve :: [Command] -> Int
solve = res . foldl solveFold initState
  where res State{x=x', y=y'} = abs x' + abs y'

solveFold :: State -> Command -> State
solveFold State{x=x', y=y', wx=wx', wy=wy'} (action, v)
  | action == 'N'       = State {x=x',  y=y',   wx=wx',     wy=wy' - v}
  | action == 'E'       = State {x=x',  y=y',   wx=wx' + v, wy=wy'}
  | action == 'S'       = State {x=x',  y=y',   wx=wx',     wy=wy' + v}
  | action == 'W'       = State {x=x',  y=y',   wx=wx' - v, wy=wy'}
  | action `elem` "LR"  = State {x=x',  y=y',   wx=rotx,    wy=roty}
  | action == 'F'       = State {x=x' + v*wx',
                                 y=y' + v*wy',
                                 wx=wx', wy=wy'}
  where (rotx, roty) = apply rotRight (v' `div` 90 `mod` 4) (wx', wy')
        v' = if action == 'L' then (-v) else v
        rotRight (a, b) = (-b, a)
        apply f n = foldr (.) id (replicate n f)
