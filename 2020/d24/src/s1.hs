module S1
  ( main
  ) where

import qualified Data.HashMap.Strict           as M
import           Utils

main = do
  txt <- readFile "input.txt"
  case parseD24 txt of
    Left  err -> print err
    Right res -> do
      print $ solve res

solve :: [Tile] -> Int
solve ts = length $ M.filter odd counts
  where counts = counter $ map toCanonicalTile ts
