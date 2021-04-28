import           Control.Monad.State
import           Data.Bits
import           Data.Bool       (bool)
import qualified Data.Map.Strict as M
import           Text.Parsec     (parse)
import           Utils

type Mask = Integer -> Integer
type Memory = M.Map Integer Integer
type PuzzleState = (Mask, Memory)

startState = (id, M.empty)

main :: IO ()
main = readFile "input.txt"
  >>= print
  . M.foldr (+) 0 -- Sum.
  . flip evalState startState -- Eval.
  . solve
  . lines

-- Constructs mask functions, that map a value to its result.
makeMask :: String -> Mask
makeMask m = \x -> (x .|. makeBinaryMask '1' m) .&. complement (makeBinaryMask '0' m)
  where makeBinaryMask c = foldl1 ((+) . (2 *)) . map (bool 0 1 . (== c))

solve :: [String] -> State PuzzleState Memory
solve [] = snd <$> get -- Return the memory.
solve (s:ss) = do
  (msk, mem) <- get
  case parse command "input command" s of
    Left e -> error $ "error: " ++ show e
    Right cmd -> 
      case cmd of
        CmdSetMask m  -> put $ (makeMask m, mem) -- Update the mask.
        CmdSetMem k v -> put $ (msk, M.insert k (msk v) mem) -- Update memory.
  solve ss
