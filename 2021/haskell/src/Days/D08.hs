-- Handspun logic. We know which notes are associated with 1, 4, 7, and 8. Use
-- the two segments of 1 to find 2, 5, and 6. 2 is the only one without the
-- lower 1 segment. Similarly, 5 and 6 are the only ones without the upper
-- segment. The remaining five-segment number is 3. Finally determine 0 and 9 by
-- checking the number of intersections with 3: 0 has four intersections, 9 has
-- five intersections.

module Days.D08 where

import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Map                      as M
import           Utils


parse :: String -> ([[String]], [[String]])
parse s = (map words notes, map words output)
 where
  (notes, output) = s |> lines 
                      |> map (splitOn " | " .> (\[x, y] -> (x, y))) 
                      |> unzip

part1 :: ([[String]], [[String]]) -> Int
part1 (_, out) = out |> concat |> count (length .> (`elem` [2, 3, 4, 7]))

part2 :: ([[String]], [[String]]) -> Int
part2 = uncurry zip .> map decode .> sum

decode :: ([String], [String]) -> Int
decode (notes, out) = out |> map (sort .> (res M.!) .> intToDigit) |> read
  where res = sherlock notes |> M.mapKeys sort

sherlock :: [String] -> M.Map String Int
sherlock notes = m2
 where
  m0 = simpleSherlock notes
  fromDigit d m = invMap m M.! d
  [lCF1, lCF2]   = fromDigit 1 m0
  hasNoCF1       = filter (lCF1 `notElem`) notes
  hasNoCF2       = filter (lCF2 `notElem`) notes
  (d2, hasNoCF0) = if length hasNoCF1 == 1
    then (head hasNoCF1, hasNoCF2)
    else (head hasNoCF2, hasNoCF1)
  [d5, d6] = if length (head hasNoCF0) == 5 then hasNoCF0 else reverse hasNoCF0
  m1       = M.fromList $ M.toList m0 ++ [(d2, 2), (d5, 5), (d6, 6)]
  d3       = firstBy (\n -> length n == 5 && n `notElem` M.keys m1) notes
  d0d9     = filter (`notElem` (d3 : M.keys m1)) notes
  d9       = firstBy (\n -> length (n `intersect` d3) == 5) d0d9
  d0       = firstBy (\n -> length (n `intersect` d3) == 4) d0d9
  m2       = M.fromList $ M.toList m1 ++ [(d3, 3), (d0, 0), (d9, 9)]

invMap :: (Ord k, Ord v) => M.Map k v -> M.Map v k
invMap = M.toList .> map (\(x, y) -> (y, x)) .> M.fromList

simpleSherlock :: [String] -> M.Map String Int
simpleSherlock = foldr
  (\note m -> case length note `elemIndex` lights of
    Nothing -> m
    Just i  -> M.insert note (digits !! i) m
  )
  mempty
  where (digits, lights) = unzip [(1, 2), (7, 3), (4, 4), (8, 7)]
