import           Text.Parsec
import           Text.Parsec.String

type Deck = [Int]

main = readFile "input.txt" >>= print . run . parseD22

run :: (Deck, Deck) -> Int
run ds = if null p1 || null p2 then eval next else run next
  where next@(p1, p2) = step ds

step :: (Deck, Deck) -> (Deck, Deck)
step (x : xs, y : ys)
  | x > y     = (xs ++ [x, y], ys)
  | y > x     = (xs, ys ++ [y, x])
  | otherwise = error "top cards are equal, behaviour undefined"
step d@(_, _) = d

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
