module Utils (Range, Field, Ticket, Notes(..), notes, inRange, valid) where

import Text.Parsec

type Range = (Int, Int)
type Field = (String, [Range])
type Ticket = [Int]
data Notes = Notes [Field] Ticket [Ticket] deriving (Show)

number :: Parsec String () Int
number = read <$> many1 digit

word :: Parsec String () String
word = many1 letter

range :: Parsec String () Range
range = (,) <$> number <* char '-' <*> number

field :: Parsec String () Field
field = (,) <$> label <* string ": " <*> (range `sepBy` string " or ")
    where label = concat <$> ((:) <$> word <*> many (spaces *> word))

ticket :: Parsec String () Ticket
ticket = number `sepBy1` char ','

notes :: Parsec String () Notes
notes = Notes <$> fields <*> myTicket <*> nearbyTickets
  where fields = field `sepEndBy1` char '\n'
        myTicket = spaces *> string "your ticket:" *> spaces *> ticket
        nearbyTickets = spaces *> string "nearby tickets:" *> many1 (spaces *> ticket)

-- Checks if a number is within an inclusive range.
inRange :: Int -> Range -> Bool
inRange v (l, r) = l <= v && v <= r

-- Checks if a number is valid for a field.
valid :: Int -> Field -> Bool
valid n f = any (inRange n) (snd f)