import Prelude
import qualified Data.Set as S
import Data.List.Split

requirements = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main :: IO ()
main = readFile "input.txt" >>= print . solve

solve :: String -> Int
solve = length . filter isValid . parse

isValid :: S.Set String -> Bool
isValid s = all (`S.member` s) requirements

-- Returns a set of keys.
parse :: String -> [S.Set String]
parse = map (S.fromList . map (head . splitOn ":") . splitOneOf " \n") . splitOn "\n\n"
