import           Text.Parsec
import           Text.Parsec.String

type Deck = [Int]

main = readFile "input.txt" >>= print . eval . stepUntilFinish . parseD22

stepUntilFinish :: (Deck, Deck) -> (Deck, Deck)
stepUntilFinish ds = stepUntilFinishImpl ds []
 where
  stepUntilFinishImpl ds@(x : xs, y : ys) history
    | null p1 || null p2 = next
    | ds `elem` history  = (p1 ++ p2, [])
    | otherwise          = stepUntilFinishImpl next (ds : history)
   where
    next@(p1, p2)
      | length xs >= x && length ys >= y = if p1WinsSubmatch
        then p1Wins
        else p2Wins
      | x > y = p1Wins
      | y > x = p2Wins
      | otherwise = error "top cards are equal, behaviour undefined"
    p1Wins         = (xs ++ [x, y], ys)
    p2Wins         = (xs, ys ++ [y, x])
    p1WinsSubmatch = null . snd $ stepUntilFinish (take x xs, take y ys)
  stepUntilFinishImpl ds@(_, _) history = ds

eval :: (Deck, Deck) -> Int
eval (p1, p2) = max (cal p1) (cal p2)
  where cal = sum . zipWith (*) [1 ..] . reverse

parseD22 :: String -> (Deck, Deck)
parseD22 s = case parse ((,) <$> deck <* spaces <*> deck) "" s of
  Left  err -> error $ show err
  Right res -> res

deck :: Parser Deck
deck = do
  string "Player "
  number
  char ':'
  newline
  number `endBy1` spaces
  where number = read <$> many1 digit :: Parser Int
