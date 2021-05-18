import           Data.Either        (fromRight)
import           Text.Parsec
import           Text.Parsec.String

data Symbol = Value Int | Operation (Int -> Int -> Int) | Parenthesised [Symbol]
type Expression = [Symbol]

main :: IO ()
main = readFile "input.txt" >>= print . sum . map (eval . fromRight undefined . parse expression "") . lines

number :: Parser Int
number = read <$> many1 digit

expression :: Parser Expression
expression = many1 $ choice [
    try $ Value <$> (spaces *> number),
    try $ Operation <$> (spaces *> choice [(+) <$ string "+", (*) <$ string "*"]),
    try $ Parenthesised <$> (spaces *> between (char '(') (char ')') expression)
  ]

eval :: Expression -> Int
eval (x:xs) = go xs $ result x
  where go [] acc                    = acc
        go (Operation op : y:ys) acc = go ys $ op acc $ result y

        result (Value v)         = v
        result (Parenthesised p) = eval p
