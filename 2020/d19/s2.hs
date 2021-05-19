import           Control.Applicative          (liftA2)
import           Data.Char                    (isAlpha)
import           Data.Either                  (fromRight, isRight)
import qualified Data.HashMap.Strict          as M
import           Text.Parsec
import           Text.Parsec.String           (Parser)
import qualified Text.ParserCombinators.ReadP as P

type Label = Int
data Rule = Ref [[Label]] | Raw String deriving (Show)
data D19Data = D19Data (M.HashMap Label Rule) [String] deriving (Show)

main :: IO ()
main = do
  text <- readFile "input.txt"
  case parse d19 "" text of
    Left err -> print err
    Right d  -> print $ eval $ part2Update d

part2Update :: D19Data -> D19Data
part2Update (D19Data rs ms) = D19Data (M.fromList newRules `M.union` rs) ms
  where newRules = [(8, Ref [[42], [42, 8]]),
                    (11, Ref [[42, 31], [42, 11, 31]])]

rule :: Parser (Label, Rule)
rule = (,) <$> number <* string ": " <*> (raw <|> ref)
  where ref = Ref <$> ptrn `sepBy` string "| "  -- Reference of rules (e.g. 2 3 | 3 2)
        ptrn = number `sepEndBy` char ' '
        raw = Raw <$> try (spaces *> between (char '"') (char '"') (many1 $ satisfy isAlpha)) -- Raw string (e.g. "a")

msg :: Parser String
msg = many1 $ satisfy isAlpha

d19 :: Parser D19Data
d19 = D19Data <$> (M.fromList <$> rule `endBy` endOfLine) <* spaces <*> msg `sepBy` spaces

number :: Parser Int
number = read <$> many1 digit

eval :: D19Data -> Int
eval (D19Data rules messages) = length $ filter check messages
  where check = not . null . P.readP_to_S p
        p = buildParser rules

buildParser :: M.HashMap Label Rule -> P.ReadP ()
buildParser rs = parseAt 0 *> P.eof
  where parseRule (Ref lss) = P.choice $ map parseSequence lss
        parseRule (Raw s)   = P.string s
        parseSequence = mapM parseAt
        parseAt idx = head <$> parseRule (rs M.! idx)
