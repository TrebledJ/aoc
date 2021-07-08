import qualified Data.HashSet                  as S
import           Data.List
import qualified Data.Map.Strict               as M
import           Utils

main = do
  txt <- readFile "input.txt"
  let fs = parseD21 txt
      is = ingredientSet fs
      as = allergenSet fs

  putStrLn $ solve fs is as

solve :: [Food] -> S.HashSet Ingredient -> S.HashSet Allergen -> String
solve fs is as = intercalate "," . map snd . sort $ M.toList algMap
 where
  nonAllergen = is `S.difference` S.fromList (M.elems algMap)
  algMap      = makeAllergenMap fs is as
