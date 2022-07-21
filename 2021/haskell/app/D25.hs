{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ImplicitParams #-}

module D25 where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.STRef
import qualified Data.Vector.Unboxed           as V
import qualified Data.Vector.Unboxed.Mutable   as MV
import           Utils


type Cell = Char
type IGrid = UArray (Int, Int) Cell


main :: IO ()
main = defaultMain defaultFile parse part1 part2

defaultFile :: String
defaultFile = "../input/d25.txt"

parse :: String -> (IGrid, Int, Int, V.Vector (Int, Int), V.Vector (Int, Int))
parse inp = (g, w, h, rs, ds)
 where
  ls = lines inp
  h  = length ls
  w  = length $ head ls
  g  = listArray ((0, 0), (h - 1, w - 1)) (concat ls)
  rs = V.fromList $ map fst $ filter ((== '>') . snd) $ assocs g
  ds = V.fromList $ map fst $ filter ((== 'v') . snd) $ assocs g


debugM :: Monad m => String -> m ()
debugM s = trace s (return ())


{-
  Implementation Note:

  We use V.Vector (Int, Int) to store `rights` and `downs` since
  unboxed arrays can't store (Int, Int).

  https://stackoverflow.com/q/13461020/10239789
-}
part1 :: (IGrid, Int, Int, V.Vector (Int, Int), V.Vector (Int, Int)) -> Int
part1 (g, w, h, rs, ds) = part1' (g, rs, ds, 1)
 where
  part1' (g, rs, ds, i) = runST $ do
    moved <- newSTRef False
    g'    <- thaw g :: ST s (STUArray s (Int, Int) Cell)
    rs'   <- V.thaw rs
    ds'   <- V.thaw ds

    forM_ [0 .. MV.length rs' - 1] $ \ind -> do
      let cur = rs V.! ind
      let tar = eastof cur
      when (isfree g tar) $ do
        writeArray g' cur '.'
        writeArray g' tar '>'
        MV.write rs' ind tar
        writeSTRef moved True

    g <- freeze g'
    forM_ [0 .. MV.length ds' - 1] $ \ind -> do
      let cur = ds V.! ind
      let tar = southof cur
      when (isfree g tar) $ do
        writeArray g' cur '.'
        writeArray g' tar 'v'
        MV.write ds' ind tar
        writeSTRef moved True

    moved' <- readSTRef moved
    if moved'
      then do
        g  <- freeze g'
        -- debugM $ "iter" ++$ i ++ "\n" ++ disp w h g ++ "\n"
        rs <- V.freeze rs'
        ds <- V.freeze ds'
        return $ part1' (g, rs, ds, i + 1)
      else do
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
