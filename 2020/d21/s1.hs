import qualified Data.HashSet                  as S
import           Data.List
import qualified Data.Map.Strict               as M
import           Utils

main = do
  txt <- readFile "input.txt"
  let fs = parseD21 txt
      is = ingredientSet fs
      as = allergenSet fs

  print $ solve fs is as

solve :: [Food] -> S.HashSet Ingredient -> S.HashSet Allergen -> Int
solve fs is as = sum
  $ map (length . S.intersection nonAllergen . S.fromList . ingredients) fs
 where
  nonAllergen = is `S.difference` S.fromList (M.elems algMap)
  algMap      = makeAllergenMap fs is as
