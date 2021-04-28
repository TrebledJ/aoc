module Utils (Command(..), command) where

import Text.Parsec

data Command =
  CmdSetMask String |
  CmdSetMem Integer Integer
  deriving (Show)

eq :: Parsec String () ()
eq = spaces *> char '=' *> spaces

mask :: Parsec String () String
mask = many1 $ char '1' <|> char '0' <|> char 'X'

number :: Parsec String () Integer
number = read <$> many1 digit

brackets :: Parsec String u a -> Parsec String u a
brackets = between (char '[') (char ']')

command :: Parsec String () Command
command = choice [
    try $ CmdSetMem <$> (string "mem" *> brackets number) <*> (eq *> number),
    try $ CmdSetMask <$> (string "mask" *> eq *> mask)
  ]
