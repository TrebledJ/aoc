import qualified Data.HashMap.Strict           as M
import           Text.Parsec
import           Text.Parsec.String

data Tile = Tile Int [[Bool]]
  deriving Show
type Edges = (Int, Int, Int, Int)
data EncodedTile = ETile
  { tileId :: Int
  , edges  :: Edges
  }
  deriving Show


main = do
  txt <- readFile "input.txt"
  case parse (sepBy1 encodedTile spaces) "" txt of
    Left  err -> print err
    -- Right res -> print $ length res
    Right res -> do
      let occurrences = counter $ concatMap (edgeToList . edges) res -- Count occurrences of each edge.
          corners     = filterTilesWithUniqueEdges occurrences 2 res -- Find tiles with 2 unique edges.

      print $ map tileId corners

      if length corners /= 4
        then putStrLn "Could not find the 4 corners."
        else print . product . map tileId $ corners

-- Filters tiles with a number of unique edges.
filterTilesWithUniqueEdges
  :: M.HashMap Int Int -> Int -> [EncodedTile] -> [EncodedTile]
filterTilesWithUniqueEdges m n = filter ((== n) . countUnique)
 where
  countUnique (ETile id es) = length . filter isUnique $ edgeToList es -- Number of unique edges.
  isUnique edge = (m M.! edge) == 1 -- An unmatched edge means that the edge is unique within the map.

-- Counts the occurrences of each element in the list.
counter :: [Int] -> M.HashMap Int Int
counter = foldr (\x acc -> M.insertWith (+) x 1 acc) M.empty

edgeToList :: Edges -> [Int]
edgeToList (n, e, s, w) = [n, e, s, w]

encodedTile :: Parser EncodedTile
encodedTile = tileToEncoded <$> tile

tile :: Parser Tile
tile = do
  string "Tile "
  id <- read <$> many1 digit
  string ":\n"
  grid <- endBy1 row (char '\n')
  return $ Tile id grid
 where
  row = many1 bit
  bit = choice [False <$ char '.', True <$ char '#']

tileToEncoded :: Tile -> EncodedTile
tileToEncoded (Tile id xs) = ETile id (make n, make e, make s, make w)
 where
  n = head xs
  e = map last xs
  s = reverse $ last xs
  w = reverse $ map head xs
  make bs = min (bitsToInt bs) (bitsToInt $ reverse bs) -- BS indeed. Greedy matching. Here we don't 
                                                        -- check if it's consistent with tile rotations/flips.

bitsToInt :: [Bool] -> Int
bitsToInt = sum . zipWith (\i b -> fromEnum b * 2 ^ i) [0 ..]
