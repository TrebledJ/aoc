{-# LANGUAGE FlexibleContexts #-}

import           Data.Either        (fromRight)
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.String (Parser)

main :: IO ()
main = readFile "input.txt" >>= print . sum . map (fromRight undefined . parse expr "") . lines

expr :: Parser Int
expr = buildExpressionParser table term
  where term  =  spaces *> (parens expr <|> number)
        table = [ [binary '+' (+) AssocLeft]
                , [binary '*' (*) AssocLeft]
                ]
        parens = between (char '(') (char ')')
        binary name fun assoc = Infix (fun <$ try (spaces *> char name)) assoc
        number = read <$> many1 digit
