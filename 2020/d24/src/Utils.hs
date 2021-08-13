{-# LANGUAGE DeriveGeneric #-}

module Utils
  ( Direction(..)
  , Tile
  , CanonicalTile
  , toCanonicalTile
  , allDirs
  , getAdjacent
  , parseD24
  , counter
  ) where

import qualified Data.HashMap.Strict           as M
import           Data.Hashable                  ( Hashable )
import           GHC.Generics                   ( Generic )
import           Text.Parsec
import           Text.Parsec.String             ( Parser )


data Direction = W | NW | NE | E | SE | SW deriving (Show, Eq, Enum, Bounded, Generic)
type Tile = [Direction]
type CanonicalTile = (Int, Int) -- We'll represent the "uniquely identifiable" tile as a pair.
                                -- The fields represent the magnitude in the NW and E directions
                                -- respectively. This will serve as our "basis vector".

instance Hashable Direction


toCanonicalTile :: Tile -> CanonicalTile
toCanonicalTile t = (nw + ne, e + ne)
 where
  mag = counter t
  get map dir = M.findWithDefault 0 dir map
  nw = mag `get` NW - mag `get` SE
  ne = mag `get` NE - mag `get` SW
  e  = mag `get` E - mag `get` W

allDirs :: [Direction]
allDirs = [minBound .. maxBound]

getAdjacent :: CanonicalTile -> Direction -> CanonicalTile
getAdjacent (nw, e) dir = (nw + nw', e + e')
 where
  (nw', e') = offset dir
  offset W  = (0, -1)
  offset NW = (1, 0)
  offset NE = (1, 1)
  offset E  = (0, 1)
  offset SE = (-1, 0)
  offset SW = (-1, -1)

parseD24 :: String -> Either ParseError [Tile]
parseD24 = parse d24 ""

d24 :: Parser [Tile]
d24 = tile `sepBy1` newline

tile :: Parser Tile
tile = many1 $ choice
  [ try $ NW <$ string "nw"
  , try $ NE <$ string "ne"
  , try $ SW <$ string "sw"
  , try $ SE <$ string "se"
  , try $ W <$ string "w"
  , try $ E <$ string "e"
  ]

counter :: (Hashable k, Eq k) => [k] -> M.HashMap k Int
counter = foldr (\x -> M.insertWith (+) x 1) mempty
