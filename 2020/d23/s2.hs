{-# LANGUAGE FlexibleContexts #-}

import           Data.Array.IO
import           Data.Char                      ( digitToInt )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )

-- Maps a cup id to the next cup id.
-- We'll use a mutable, unboxed array since we have lots of data.
type Cups = IOUArray Int Int

moves = 10000000
maxCups = 1000000

main = do
  xs <- map digitToInt <$> readFile "input.txt"
  array <- newListArray (1, maxCups) $ initList xs :: IO (IOUArray Int Int)
  mApplyN moves (uncurry step) (array, head xs)
  print =<< eval array
  return ()

initList :: [Int] -> [Int]
initList xs = front ++ [end + 1 .. maxCups] ++ [head xs]
 where
  front =
    [ let i = fromJust (x `elemIndex` xs)
      in  if i == length xs - 1 then end else xs !! ((i + 1) `mod` length xs)
    | x <- [1 .. length xs]
    ]
  end = if maxCups <= length xs then head xs else length xs + 1 -- Where the last cup in the input maps to.

eval :: (MArray a Int m) => a Int Int -> m Int
eval array = do
  x <- readArray array 1
  y <- readArray array x
  return $ x * y

step :: (MArray a Int m) => a Int Int -> Int -> m (a Int Int, Int)
step array curr = do
  movingCups <- traverse (\n -> mApplyN n (readArray array) curr) [1 .. 3]

  let sub1AndMod n = if n == 1 then maxCups else n - 1
      tryDest d = if d `elem` movingCups then tryDest (sub1AndMod d) else d
      dest = tryDest (sub1AndMod curr)

  writeArray array curr =<< readArray array (last movingCups)
  writeArray array (last movingCups) =<< readArray array dest
  writeArray array dest (head movingCups)

  next <- readArray array curr -- Next cup.
  return (array, next)

applyN :: Int -> (a -> a) -> a -> a
applyN n f = (!! n) . iterate f

mApplyN :: (Monad m) => Int -> (a -> m a) -> a -> m a
mApplyN n f x
  | n < 0 = error
    "quantum time warping activated, applying function negative times"
  | n == 0 = return x
  | otherwise = mApplyN (n - 1) f =<< f x
