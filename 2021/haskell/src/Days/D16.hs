module Days.D16 where

import           Numeric                        ( showIntAtBase )
import           Text.Megaparsec         hiding ( parse )
import           Text.Megaparsec.Char
import           Text.Printf                    ( printf )
import           Utils                   hiding ( count )


data Packet = Packet Int Int PacketObj deriving Show
data PacketObj = Literal Int | Operands [Packet] deriving Show


main :: IO ()
main = defaultMain defaultFile topLevelParse part1 part2

defaultFile :: String
defaultFile = "../input/d16.txt"

topLevelParse :: String -> Packet
topLevelParse = parse packet . transform
 where
  transform = concatMap (\h -> toBinary $ read $ "0x" ++ [h])
  toBinary n = printf "%04s" (showIntAtBase 2 ("01" !!) n "") -- A LUT is probably better.

parse :: Parser a -> String -> a
parse p s = case runParser (p <* many (char '0')) "" s of
  Right res -> res
  Left  err -> trace (errorBundlePretty err) undefined

packet :: Parser Packet
packet = do
  version <- fromBinary <$> bits 3
  typeID  <- fromBinary <$> bits 3
  if typeID == 4
    then do
      bs <- collectWhile (bits 5) $ \(b : _) -> b == '1'
      return $ Packet version typeID $ Literal $ fromBinary $ concatMap (drop 1) bs
    else do
      lenTypeID <- fromBinary <$> bits 1
      children <- operands lenTypeID
      return $ Packet version typeID (Operands children)
 where
  collectWhile p f = do -- Parse while condition is true.
    x <- p
    if f x then (x :) <$> collectWhile p f else return [x]

operands :: Int -> Parser [Packet]
operands 0 = do
  len      <- fromBinary <$> bits 15
  parse (some packet) <$> bits len
operands _ = do
  num      <- fromBinary <$> bits 11
  count num packet

bits :: Int -> Parser String
bits n = count n digitChar -- Parses n digits (assume to be bits).

part1 :: Packet -> Int
part1 (Packet v _ obj) = case obj of
  Literal _ -> v
  Operands ps -> v + sum (map part1 ps)

part2 :: Packet -> Int
part2 (Packet _ op obj) = case obj of
  Literal x -> x
  Operands ps -> case op of
      0 -> sum
      1 -> product
      2 -> minimum
      3 -> maximum
      5 -> luncurry (>)
      6 -> luncurry (<)
      7 -> luncurry (==)
      _ -> undefined
    $ map part2 ps
 where
  luncurry op' [a, b] = if op' a b then 1 else 0
  luncurry _   _      = undefined
