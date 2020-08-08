-- runhaskell s2.hs < input.txt

import Data.Tree

type Satellite = String
type STree = Tree Satellite
type Orbit = (Satellite, Satellite)

main :: IO ()
main = interact $ (++ "\n") . show . findMinimumTransfers "YOU" "SAN" . fromOrbits . map parseOrbit . lines

parseOrbit :: String -> Orbit
parseOrbit s = (takeWhile (/= ')') s, tail $ dropWhile (/= ')') s)

fromOrbits :: [Orbit] -> STree
fromOrbits orbits = construct "COM"
    where construct :: Satellite -> STree
          construct root = Node { rootLabel = root, subForest = map construct $ children root }

          children :: Satellite -> [Satellite]
          children sat = map snd $ filter ((== sat) . fst) orbits

-- finds the minimum number of orbital transfers required between two targets
findMinimumTransfers :: Satellite -> Satellite -> STree -> Int
findMinimumTransfers tar tar' = findImpl 0
    where -- find the common node where targets are (possibly indirect) children
          findImpl :: Int -> STree -> Int
          findImpl depth (Node rootLabel subForest)
                | rootLabel == tar || rootLabel == tar' = depth - 1
                | length subForest == 0 = 0
                | otherwise = 
                    let childResults = filter (/= 0) $ map (findImpl (depth + 1)) subForest
                    in  if length childResults == 2
                        then sum childResults - (depth * length childResults)   --  found common node
                        else sum childResults   --  propagate results
