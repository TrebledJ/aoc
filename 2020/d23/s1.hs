import           Data.Char
import           Data.List
import           Data.Maybe

type Cups = [Int]

main = do
  cups <- map digitToInt <$> readFile "input.txt"
  putStrLn $ eval . fst $ applyN 100 (uncurry move) (cups, 0)
  return ()

eval :: Cups -> String
eval cs = drop 1 $ map intToDigit $ take size $ dropWhile (/= 1) $ cycle cs
  where size = length cs

move :: Cups -> Int -> (Cups, Int)
move cs curridx =
  ( rotate ((curridx - fromJust (curr `elemIndex` nextcs)) `mod` size) nextcs
  , (curridx + 1) `mod` size
  )
 where
  nextcs =
    let (pre, post) = break (== dest) rem
    in  pre ++ head post : cut ++ drop 1 post
  curr = cs !! curridx
  dest = tryDest $ (curr - 1) `mod1` size
  tryDest d = if d `elem` rem then d else tryDest $ (d - 1) `mod1` size
  cut = take takeSize . drop 1 . snd $ splat -- The cups taken out.
  rem =
    take (size - takeSize)
      $  drop (takeSize - (size - curridx - 1)) (fst splat)
      ++ curr
      :  drop (takeSize + 1) (snd splat) -- The remaining cups concatenated.
  splat    = splitAt curridx $ cycle cs -- splitted --> "splat"
  size     = length cs
  takeSize = 3

-- 1-based index mod function.
mod1 :: Integral a => a -> a -> a
x `mod1` n = (+ 1) . (`mod` n) . (\x -> x - 1) $ x

applyN :: Int -> (a -> a) -> a -> a
applyN n f = (!! n) . iterate f

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith const (drop (length xs - n) (cycle xs)) xs
