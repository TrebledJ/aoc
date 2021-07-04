module Utils
  ( sqrti
  , applyN
  , rotateGridAsList
  , rotateGrid
  , rotate
  , count
  ) where

import qualified Data.Vector                   as V

sqrti :: Int -> Int
sqrti = round . sqrt . fromIntegral

applyN :: Int -> (a -> a) -> a -> a
applyN n f = (!! n) . iterate f

listToVector :: [[a]] -> V.Vector (V.Vector a)
listToVector = V.fromList . map V.fromList

vectorToList :: V.Vector (V.Vector a) -> [[a]]
vectorToList = map V.toList . V.toList

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith const (drop (length xs - n) (cycle xs)) xs

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- Rotates a 2D list clockwise.
rotateGridAsList :: [[a]] -> [[a]]
rotateGridAsList = vectorToList . rotateGrid . listToVector

-- Rotates a 2D vector clockwise.
rotateGrid :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
rotateGrid g = V.fromList
  [ V.fromList [ g V.! (length g - j - 1) V.! i | (i, j) <- row ]
  | row <- mesh
  ]
 where
  mesh = map (flip map [0 .. V.maximum (V.map length g) - 1] . (,))
             [0 .. length g - 1]
