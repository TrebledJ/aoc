import           Control.Monad.State
import           Data.Bits
import           Data.Bool       (bool)
import           Data.Char       (intToDigit, digitToInt, isDigit)
import qualified Data.Map.Strict as M
import           Data.Maybe      (listToMaybe, fromJust)
import           Numeric         (showIntAtBase, readInt)
import           Text.Parsec     (parse)
import           Utils

type Mask = Integer -> String
type Memory = M.Map Integer Integer
type PuzzleState = (Mask, Memory)

startState = (show, M.empty)

main :: IO ()
main = readFile "input.txt"
  >>= print
  . M.foldr (+) 0
  . flip evalState startState
  . solve
  . lines

binary :: Integer -> String
binary n = showIntAtBase 2 intToDigit n ""

binaryPadded :: Integer -> Int -> String
binaryPadded n len = replicate (len - length b) '0' ++ b
  where b = binary n

decimal :: String -> Integer
decimal = fromJust . fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

-- Constructs a mask (containing 'X's).
makeMask :: String -> Mask
makeMask m x = zipWith (\b b' -> bool b 'X' (b' == 'X')) withOnes m
  where makeBinaryMask c = foldl1 ((+) . (2 *)) . map (bool 0 1 . (== c))
        withOnes = binaryPadded (x .|. makeBinaryMask '1' m) (length m)

-- Generates all keys for a given decoded memory address.
generateKeys :: String -> [Integer]
generateKeys m = map (decimal . generateKeyAt) [0..2^numX - 1]
  where generateKeyAt i = substituteX m $ binaryPadded i numX
        substituteX [] _ = []
        substituteX ('X':ss) (x:xs) = x:substituteX ss xs
        substituteX (s:ss) xs = s:substituteX ss xs
        numX = length $ filter (== 'X') m

solve :: [String] -> State PuzzleState Memory
solve [] = snd <$> get
solve (s:ss) = do
  (msk, mem) <- get
  case parse command "input command" s of
    Left e -> error $ "error: " ++ show e
    Right cmd -> 
      case cmd of
        CmdSetMask m  -> put $ (makeMask m, mem) -- Update mask.

        -- Update all keys based on mask.
        CmdSetMem k v -> put $ (msk, foldr (\k' mem -> M.insert k' v mem) mem . generateKeys $ msk k)
  solve ss
