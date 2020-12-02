import Prelude
import Data.Text (pack, unpack, split)
import Text.Regex.TDFA  -- regex-tdfa


parse :: String -> (Int, Int, Char, String)
parse s = (min, max, head (unpack c), tail (unpack r))
  where [l, r] = split (==':') $ pack s
        [range, c] = split (==' ') l
        [min, max] = map (read . unpack) $ split (=='-') range
        
parse2 :: String -> (Int, Int, Char, String)
parse2 s = (read m, read n, head c, text)
  where regex = "([0-9]+)-([0-9]+) (.): (.+)"
        (_, _, _, [m, n, c, text]) = s =~ regex :: (String, String, String, [String])

xor :: Bool -> Bool -> Bool
xor = (/=)

check :: String -> Bool
check s = (a == c) `xor` (b == c)
  where (i, j, c, text) = parse2 s
        a = text !! (i - 1)
        b = text !! (j - 1)

main :: IO ()
main = do
  text <- lines <$> readFile "input.txt"
  print $ length $ filter check text
  return ()
