import           Control.Applicative (liftA2)
import           Data.Char           (isAlpha)
import           Data.Either         (isRight)
import qualified Data.HashMap.Strict as M
import           Text.Parsec
import           Text.Parsec.String  (Parser)

type Label = Int
data Rule = Ref [[Label]] | Raw String deriving (Show)
data D19Data = D19Data (M.HashMap Label Rule) [String] deriving (Show)

main :: IO ()
main = do
  text <- readFile "input.txt"
  case parse d19 "" text of
    Left err -> print err
    Right d  -> print $ eval d

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
  where check s = isRight $ parse p "" s
        p = buildParser rules

buildParser :: M.HashMap Label Rule -> Parser ()
buildParser rs = parseAt 0 *> eof
  where parseRule (Ref lss) = choice $ map (try . parseSequence) lss
        parseRule (Raw s)   = string s
        parseSequence = mapM parseAt
        parseAt idx = head <$> parseRule (rs M.! idx)
