import Prelude
import qualified Data.Map.Strict as M
import Data.List.Split
import Data.Char

type Passport = M.Map String String

requirements = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

main :: IO ()
main = readFile "input.txt" >>= print . solve

solve :: String -> Int
solve = length . filter isValid . parse

isValid :: Passport -> Bool
isValid pp = exists && validFields
  where exists = all (`M.member` pp) requirements
        validFields = all (uncurry isValidField) $ M.assocs pp

parse :: String -> [Passport]
parse = map (M.fromList . map (fmap (drop 1) . break (==':')) . splitOneOf " \n") . splitOn "\n\n"

between :: Int -> Int -> Int -> Bool
between a b v = a <= v && v <= b

isHex :: Char -> Bool
isHex = (`elem` "0123456789abcdef")

isValidField :: String -> String -> Bool
isValidField f s
  | f == "byr" = d 4 && between 1920 2002 r
  | f == "iyr" = d 4 && between 2010 2020 r
  | f == "eyr" = d 4 && between 2020 2030 r
  | f == "hgt" = let unit = reverse $ take 2 rev
                     val = read $ reverse $ drop 2 rev
                     rev = reverse s
                 in
                 len > 2 && unit `elem` ["in", "cm"] 
                  && if unit == "in" 
                     then between 59 76 val
                     else between 150 193 val
  | f == "hcl" = len == 7 && all isHex (tail s)
  | f == "ecl" = s `elem` eyeColors
  | f == "pid" = d 9
  | f == "cid" = True
  | otherwise = True
  where len = length s
        r = read s :: Int
        d n = len == n && all isDigit s
