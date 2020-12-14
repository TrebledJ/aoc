import Prelude
import qualified Data.HashMap.Strict as M
import Text.Regex.PCRE  -- Using PCRE here so that we get that non-greedy +? syntax.


main :: IO ()
main = readFile "input.txt" >>= print . solve . lines

solve :: [String] -> Int
solve = count "shiny gold" . M.fromList . map parse

-- Returns a (key, [children]) pair.
parse :: String -> (String, [(String, Int)])
parse x = (head matches, if x =~ "no other" then [] else map splitFields $ tail matches)
  where matches = map removeBag $ getAllTextMatches (x =~ "([0-9]* )?([a-z ]+?) bag") :: [String]
        removeBag x = take (length x - 4) x   -- Removes " bag" from the end. Lookahead doesn't work for some reason?
        splitFields = (\(a, b) -> (b, read a)) . fmap (drop 1) . break (== ' ')

count :: String -> M.HashMap String [(String, Int)] -> Int
count s m = countImpl s - 1   -- Minus 1 because we don't count the gold bag itself.
  where countImpl s = sum (map (\(child, num) -> num * countImpl child) (m M.! s)) + 1
