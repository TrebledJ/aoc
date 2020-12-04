import Prelude
import qualified Data.Set as S
import Data.List.Split


requirements = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid :: S.Set String -> Bool
isValid s = all (`S.member` s) requirements

-- Returns a set of keys
parse :: String -> [S.Set String]
parse s = map (S.fromList . map (head . splitOn ":") . splitOneOf " \n") $ splitOn "\n\n" s

main :: IO ()
main = do
  text <- readFile "input.txt"
  print $ length $ filter isValid $ parse text
  return ()
