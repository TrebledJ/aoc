import Prelude
import Data.List
import Text.Regex.TDFA


main :: IO ()
main = readFile "input.txt" >>= print . solve . lines

solve :: [String] -> Int
solve = countAncestors "shiny gold" . map parse

-- Returns a (key, [children]) pair.
parse :: String -> (String, [String])
parse x = (head matches, map splitFields $ tail matches)
  where matches = map removeBag $ getAllTextMatches (x =~ "([0-9]* )?([a-z ]+) bag") :: [String]
        removeBag x = take (length x - 4) x   -- Removes " bag" from the end. Lookahead doesn't work for some reason?
        splitFields = drop 1 . dropWhile (/= ' ')

countAncestors :: String -> [(String, [String])] -> Int
countAncestors key = length . ancestors key

ancestors :: String -> [(String, [String])] -> [String]
ancestors key xs = nub $ concatMap (`ancestors` xs) l ++ l
  where l = foldr (\x acc -> if key `elem` snd x then fst x:acc else acc) [] xs
