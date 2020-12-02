import Prelude
import Data.Text (pack, unpack, split)
import Text.Regex.TDFA


parse :: String -> (Int, Int, Char, String)
parse s = (min, max, head (unpack c), tail (unpack r))
  where [l, r] = split (==':') $ pack s
        [range, c] = split (==' ') l
        [min, max] = map (read . unpack) $ split (=='-') range

parse2 :: String -> (Int, Int, Char, String)
parse2 s = (read m, read n, head c, text)
  where regex = "([0-9]+)-([0-9]+) (.): (.+)"
        (_, _, _, [m, n, c, text]) = s =~ regex :: (String, String, String, [String])

check :: String -> Bool
check s = min <= n && n <= max
  where (min, max, c, text) = parse2 s
        n = length $ filter (==c) text

main :: IO ()
main = do
  text <- lines <$> readFile "input.txt"
  print $ length $ filter check text
  return ()
