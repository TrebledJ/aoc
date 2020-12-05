import Prelude
import Data.List (sort)
import qualified Data.HashSet as S


type Seat = (Int, Int)
row = fst
col = snd

maxRows = 128
maxCols = 8


main :: IO ()
main = solve <$> readFile "input.txt" >>= print


solve :: String -> Int
solve = findGap . map (seatToID . parse) . lines


findGap :: [Int] -> Int
findGap = dropConsecutive . sort . missing
  where missing = S.toList . (S.fromList [from..to] `S.difference`) . S.fromList . filter (\x -> from <= x && x <= to)
        dropConsecutive xs = snd . head . filter (\(curr, next) -> curr + 1 /= next) $ zip xs (drop 1 xs)
        from = 0
        to = (maxRows - 1) * maxCols


parse :: String -> Seat
parse s = (parseRow $ take 7 s, parseCol $ drop 7 s)


parseRow :: String -> Int
parseRow = fst . parseByBSearch 'F' 'B' maxRows


parseCol :: String -> Int
parseCol = fst . parseByBSearch 'L' 'R' maxCols


-- Returns the (Low, High) obtained from the binary search.
parseByBSearch :: Char -> Char -> Int -> String -> (Int, Int)
parseByBSearch left right max = foldl walk (0, max-1)
  where walk (lo, hi) c -- Keep track of the range of possible values. Narrow it down via bsearch.
          | lo == hi    = (lo, lo)
          | c == left   = (lo, (hi + lo) `div` 2)
          | c == right  = (1 + (hi + lo) `div` 2, hi)


seatToID :: Seat -> Int
seatToID s = 8 * row s + col s
