module S2
  ( main
  ) where

import           Control.Monad.State
import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import           Data.Maybe                     ( fromMaybe )
import           System.Environment
import           Utils

type TileState = M.HashMap CanonicalTile Bool
type TileValue = Int

main = do
  txt  <- readFile "input.txt"
  args <- getArgs
  let n = if null args then 100 else read (head args) :: Int

  case parseD24 txt of
    Left  err -> print err
    Right res -> do
      print $ last $ evalState (replicateM n step) $ initState res

initState :: [Tile] -> TileState
initState ts = M.map odd counts
  where counts = counter $ map toCanonicalTile ts

black = True
white = False

step :: State TileState TileValue
step = do
  state <- get
  let
    isBlack t = fromMaybe white (state M.!? t)
    isWhite t = not $ isBlack t
    numAdjacentBlackTiles t =
      length . filter isBlack $ map (getAdjacent t) allDirs

    toWhite = M.foldrWithKey' goWhite S.empty state -- Iterate through all black tiles.
    goWhite t isBlack' acc
      | isBlack' && (adjBlack == 0 || adjBlack > 2) = S.insert t acc
      | otherwise = acc
      where adjBlack = numAdjacentBlackTiles t
    toBlack = M.foldrWithKey' goBlack S.empty state -- White tiles won't necessarily appear on the map.
                                                    -- Since they're state change is dependent on black
                                                    -- tiles, we'll get the black tiles instead then look
                                                    -- at their surroundings.
    goBlack t isBlack' acc | isBlack'  = toBeBlacks <> acc
                           | otherwise = acc
     where
      toBeBlacks = S.fromList $ filter go $ map (getAdjacent t) allDirs
      go t' = isWhite t' && numAdjacentBlackTiles t' == 2

  -- Flip the tiles.
  let newState = flip (S.foldr (`M.insert` black)) toBlack
        $ S.foldr (`M.insert` white) state toWhite

  put newState
  return $ length . M.filter (== black) $ newState

eval :: State TileState TileValue
eval = do
  state <- get
  return $ length . M.filter (== black) $ state
