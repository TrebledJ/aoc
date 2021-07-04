{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tile
  ( TileID
  , Tile(..)
  , TileMap
  , TileGrid
  , TileSide(..)
  , EdgeID
  , Edge
  , EdgeMap
  , makeTileMap
  , makeEdgeMap
  , countTilesWithSameEdge
  , rotateSide
  , getEdge
  , minimumEdgeID
  , toEdgeID
  , otherEdgeID
  , edges
  , parseTiles
  ) where

import           Data.Bits                      ( testBit )
import qualified Data.HashMap.Strict           as M
import qualified Data.Vector                   as V
import           Text.Parsec             hiding ( count )
import           Text.Parsec.String


type TileID = Int
data Tile = Tile
  { tileID   :: TileID
  , tileGrid :: TileGrid
  }
  deriving Show
type TileMap = M.HashMap TileID Tile
type TileGrid = V.Vector (V.Vector Bool)
data TileSide = North | East | South | West deriving (Enum, Show)

type EdgeID = Int
type Edge = [Bool]
type EdgeMap = M.HashMap EdgeID [TileID]


-- Creates a map from tile id to the tile data.
makeTileMap :: [Tile] -> TileMap
makeTileMap = M.fromList . map (\t -> (tileID t, t))

-- Gathers tiles that share an edge the occurrences of each element in the list.
makeEdgeMap :: [Tile] -> EdgeMap
makeEdgeMap = foldr1 (M.unionWith (++)) . map makeEdgeMapFromTile
 where
  makeEdgeMapFromTile t =
    foldr ((`M.insert` [tileID t]) . minimumEdgeID) M.empty (edges t)

-- Counts the number of tiles with the same edge id.
countTilesWithSameEdge :: EdgeMap -> EdgeID -> Int
countTilesWithSameEdge m = length . (m M.!) . minimumEdgeID

rotateSide :: Int -> TileSide -> TileSide
rotateSide n = toEnum . (`mod` 4) . (+ (4 - n)) . fromEnum -- Subtract from 4 since we want to rotate counterclockwise.

getEdge :: Tile -> Bool -> TileSide -> Int -> EdgeID
getEdge t isFlipped s n = toEdgeID $ edges t !! side
 where
  s'   = fromEnum $ rotateSide n s
  side = if isFlipped then (4 - s') `mod` 4 else s'

class EdgeRepr e where
  minimumEdgeID :: e -> EdgeID

instance EdgeRepr Edge where
  minimumEdgeID x = min (toEdgeID x) (toEdgeID $ reverse x)

instance EdgeRepr EdgeID where
  minimumEdgeID x = min x (otherEdgeID x)

toEdgeID :: Edge -> EdgeID
toEdgeID = sum . zipWith (\i b -> fromEnum b * 2 ^ i) [0 ..]

otherEdgeID :: EdgeID -> EdgeID
otherEdgeID x = toEdgeID . reverse $ map (x `testBit`) [0 .. 9]

edges :: Tile -> [Edge]
edges (Tile _ xs) = map V.toList [n, e, s, w]
 where
  n = V.head xs
  e = V.map V.last xs
  s = V.reverse $ V.last xs
  w = V.reverse $ V.map V.head xs

parseTiles :: String -> [Tile]
parseTiles s = case parse (sepBy1 tile spaces) "" s of
  Left  err -> error $ show err
  Right res -> res

tile :: Parser Tile
tile = do
  string "Tile "
  id <- read <$> many1 digit
  string ":\n"
  grid <- endBy1 row (char '\n')
  return . Tile id . V.fromList . map V.fromList $ grid
 where
  row = many1 bit
  bit = choice [False <$ char '.', True <$ char '#']
