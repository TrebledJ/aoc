import qualified Data.HashSet                  as S
import           Data.List
import qualified Data.Map.Strict               as M
import           Text.Parsec
import           Text.Parsec.String

type Ingredient = String
type Allergen = String
data Food = Food
  { ingredients :: [Ingredient]
  , allergens   :: [Allergen]
  }
  deriving Show

main = do
  txt <- readFile "input.txt"
  case parse d21 "" txt of
    Left  err -> print err
    Right fs  -> do
      let is = ingredientSet fs
          as = allergenSet fs

      print $ solve fs is as

solve :: [Food] -> S.HashSet Ingredient -> S.HashSet Allergen -> Int
solve fs is as = sum
  $ map (length . S.intersection nonAllergen . S.fromList . ingredients) fs
 where
  nonAllergen = is `S.difference` S.fromList (M.elems algMap)
  algMap      = makeAllergenMap fs is as

-- We assume that a solution (allergen -> ingredient mapping) exists.
makeAllergenMap
  :: [Food]
  -> S.HashSet Ingredient
  -> S.HashSet Allergen
  -> M.Map Allergen Ingredient
makeAllergenMap fs is as = makeAllergenMapImpl firstMap firstPartition
 where
  -- Split allergens into solved/unsolved. Keep deducing allergens until there are no unsolved.
  makeAllergenMapImpl
    :: M.Map Allergen (S.HashSet Ingredient)
    -> ([Allergen], [Allergen])
    -> M.Map Allergen Ingredient
  makeAllergenMapImpl m (solved, unsolved) = if null $ snd newPartition
    then M.map (head . S.toList) newMap
    else makeAllergenMapImpl newMap newPartition
   where
    newMap       = foldr (M.adjust (`S.difference` ding)) m unsolved
    newPartition = makePartition newMap
    ding         = S.unions $ map (m M.!) solved -- Deduced INGredients (ding ding!), the ones we're certain correspond to an allergen.

  makePartition m = partition ((== 1) . length . (m M.!)) $ S.toList as

  firstMap =
    M.fromList $ map (\alg -> (alg, possibleIngredients alg)) $ S.toList as
  firstPartition = makePartition firstMap
  possibleIngredients alg =
    foldr1 S.intersection . map (S.fromList . ingredients) $ filter
      (elem alg . allergens)
      fs -- Do a reverse lookup on the food list and merge.

ingredientSet :: [Food] -> S.HashSet Ingredient
ingredientSet = S.unions . map (S.fromList . ingredients)

allergenSet :: [Food] -> S.HashSet Allergen
allergenSet = S.unions . map (S.fromList . allergens)

d21 :: Parser [Food]
d21 = food `sepBy1` newline

food :: Parser Food
food = Food <$> word `endBy1` space <*> between
  (char '(')
  (char ')')
  (string "contains " *> word `sepBy1` string ", ")
  where word = many1 letter
