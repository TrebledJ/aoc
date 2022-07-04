module D16 where

import           Numeric                        ( showIntAtBase )
import           Text.Megaparsec         hiding ( parse )
import           Text.Megaparsec.Char
import           Text.Printf                    ( printf )
import           Utils                   hiding ( count )


type Version = Int
type TypeID = Int
data Packet = PacketLit Version TypeID Int | PacketOp Version TypeID [Packet] deriving Show


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
packet = fst <$> packetWithLen

packetWithLen :: Parser (Packet, Int)
packetWithLen = do
  pos     <- getOffset
  version <- fromBinary <$> bits 3
  typeID  <- fromBinary <$> bits 3
  if typeID == 4
    then -- Literal.
         do
      bs' <- collectWhile (bits 5) $ \(b : _) -> b == '1'
      let bs = map (drop 1) bs'
      pos2 <- getOffset
      return (PacketLit version typeID $ fromBinary $ concat bs, pos2 - pos)
    else do
      lenTypeId <- fromBinary <$> bits 1
      if lenTypeId == 0
        then -- Length-bounded.
             do
          len      <- fromBinary <$> bits 15
          children <- parse (some packet) <$> bits len
          pos2     <- getOffset
          return (PacketOp version typeID children, pos2 - pos)
        else -- Multi.
             do
          num      <- fromBinary <$> bits 11
          children <- count num packet
          pos2     <- getOffset
          return (PacketOp version typeID children, pos2 - pos)
 where
  bits :: Int -> Parser String
  bits n = count n digitChar -- Parses n digits (assume to be bits).
  collectWhile p f = do -- Parse while condition is true.
    x <- p
    if f x then (x :) <$> collectWhile p f else return [x]


part1 :: Packet -> Int
part1 (PacketLit v _ _ ) = v
part1 (PacketOp  v _ ps) = v + sum (map part1 ps)

part2 :: Packet -> Int
part2 (PacketLit _ _ x) = x
part2 (PacketOp _ op ps) =
  case op of
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
