module Days.D10 where

import           Data.List                      ( sort )
import           Data.Maybe                     ( isNothing )
import           Utils


parse :: String -> [String]
parse = lines

part1 :: [String] -> Int
part1 = sum . map (score . checkCorrupted)

part2 :: [String] -> Int
part2 = median . map (tally . map score2 . closeOff) . filter
  (isNothing . checkCorrupted)
 where
  median xs = sort xs !! (length xs `div` 2) -- Assume odd # of elements.
  tally = foldl (\acc x -> 5 * acc + x) 0

checkCorrupted :: String -> Maybe Char
checkCorrupted xs' = check xs' []
 where
  check [] _ = Nothing
  check (x : xs) st
    | x `elem` "([{<"                        = check xs (x : st)
    | not (null st) && invert (head st) == x = check xs (tail st)
    | otherwise                              = Just x

score :: Maybe Char -> Int
score (Just ')') = 3
score (Just ']') = 57
score (Just '}') = 1197
score (Just '>') = 25137
score _          = 0

closeOff :: String -> String
closeOff xs' = map invert $ close xs' []
 where
  close [] st = st
  close (x : xs) st
    | x `elem` "([{<"                        = close xs (x : st)
    | not (null st) && invert (head st) == x = close xs (tail st)
    | otherwise                              = undefined

invert :: Char -> Char
invert '(' = ')'
invert '[' = ']'
invert '{' = '}'
invert '<' = '>'
invert _   = undefined

score2 :: Char -> Int
score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4
score2 _   = undefined
