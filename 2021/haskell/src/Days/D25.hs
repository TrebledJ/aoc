{-# LANGUAGE FlexibleContexts #-}

module Days.D25 where

import           Control.Monad.ST
import qualified Criterion.Main                as C
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.STRef
import qualified Data.Vector.Unboxed           as V
import           Utils


type Cell = Char
type IGrid = UArray (Int, Int) Cell


parse :: String -> (IGrid, Int, Int, V.Vector (Int, Int), V.Vector (Int, Int))
parse inp = (g, w, h, rs, ds)
 where
  ls = inp |> lines
  h  = ls |> length
  w  = ls |> head |> length
  g  = ls |> concat |> listArray ((0, 0), (h - 1, w - 1))
  rs = g |> assocs 
         |> filter (snd .> (== '>')) 
         |> map fst 
         |> V.fromList
  ds = g |> assocs 
         |> filter (snd .> (== 'v')) 
         |> map fst 
         |> V.fromList


debugM :: Monad m => String -> m ()
debugM s = trace s (return ())


{-
  Implementation Note:

  We use V.Vector (Int, Int) to store `rights` and `downs` since unboxed arrays can't store (Int, Int).
  https://stackoverflow.com/q/13461020/10239789
-}
part1 :: (IGrid, Int, Int, V.Vector (Int, Int), V.Vector (Int, Int)) -> Int
part1 (g, w, h, rs, ds) = part1' g rs ds 1
 where
  part1' g rs ds i = runST $ do
    moved <- newSTRef False
    newg  <- thaw g :: ST s (STUArray s (Int, Int) Cell)

    rs'   <- V.forM rs $ \cur -> do
      let tar = eastof cur
      if isfree g tar
        then do
          writeArray newg cur '.' -- Replace old cell with empty.
          writeArray newg tar '>' -- Move cucumber to new cell.
          writeSTRef moved True   -- Set the moved flag.
          return tar              -- Position has changed.
        else do
          return cur              -- Position has not changed.

    g   <- freeze newg -- Copy into old g to save the state after east-facing cucumbers have moved.
    ds' <- V.forM ds $ \cur -> do
      let tar = southof cur
      if isfree g tar
        then do
          writeArray newg cur '.'
          writeArray newg tar 'v'
          writeSTRef moved True
          return tar
        else do
          return cur

    moved' <- readSTRef moved
    if moved'
      then do -- If something moved, keep looping.
        g' <- freeze newg
        return $ part1' g' rs' ds' (i + 1)
      else do -- If nothing has moved, return current iteration.
        return i

  eastof (y, x) | x + 1 == w = (y, 0)
                | otherwise  = (y, x + 1)
  southof (y, x) | y + 1 == h = (0, x)
                 | otherwise  = (y + 1, x)

  isfree :: UArray (Int, Int) Cell -> (Int, Int) -> Bool
  isfree g (y, x) = g ! (y, x) == '.'

part2 :: a -> Int
part2 _ = 0

disp :: Int -> Int -> UArray (Int, Int) Cell -> String
disp w h g = unlines [ [ g ! (j, i) | i <- [0 .. w - 1] ] | j <- [0 .. h - 1] ]
