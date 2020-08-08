-- runhaskell s1.hs < input.txt

import Data.Tree

type Satellite = String
type STree = Tree Satellite
type Orbit = (Satellite, Satellite)

main :: IO ()
main = interact $ (++ "\n") . show . countOrbits . fromOrbits . map parseOrbit . lines

parseOrbit :: String -> Orbit
parseOrbit s = (takeWhile (/= ')') s, tail $ dropWhile (/= ')') s)

fromOrbits :: [Orbit] -> STree
fromOrbits orbits = construct "COM"
    where children :: Satellite -> [Satellite]
          children sat = map snd $ filter ((== sat) . fst) orbits
        
          construct :: Satellite -> STree
          construct root = Node { rootLabel = root, subForest = map construct $ children root }

countOrbits :: STree -> Integer
countOrbits = countOrbitsImpl 0
    where countOrbitsImpl :: Integer -> STree -> Integer
          countOrbitsImpl depth (Node rootLabel subForest)
                | length subForest == 0 = depth
                | otherwise = depth + (sum $ map (countOrbitsImpl (depth + 1)) subForest)