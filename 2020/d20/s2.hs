import           Data.Bool                      ( bool )
import qualified Data.HashMap.Strict           as M
import           Data.List                      ( (\\) )
import qualified Data.Vector                   as V
import           Text.Regex.PCRE
import           Tile
import           Utils


data TileRef = NullTileRef | TileRef
  { tileRefID       :: TileID
  , tileRefFlipped  :: Bool
  , tileRefRotation :: Int
  }
  deriving Show
data Constraint = NoConstraint | UniqueEdgeConstraint | MatchConstraint EdgeID deriving (Eq, Show)
type TileConstraint = [Constraint]


-- Length of a tile side.
tileSize = 10

monsterLiteral =
  "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "

main = do
  txt <- readFile "input.txt"
  let tiles = parseTiles txt

  -- Helper functions
  let prettyPrint = putStrLn . unlines . V.toList . V.map
        (V.toList . V.map (bool '.' '#'))

  let
    tm       = makeTileMap tiles
    em       = makeEdgeMap tiles
    gridSize = sqrti $ length tm
    pretty   = pixeliseTiles tm $ fuseTiles tm em -- Char representation of solution.
    strs     = -- Try with every rotation and flipped orientation of the grid.
               concatMap
      (\x ->
        map (unlines . applyN x rotateGridAsList) [pretty, map reverse pretty]
      )
      [0 .. 3]
    numMonsters = maximum $ map (findMonsters gridSize) strs
    hashes      = count (== '#') . head $ strs

  putStrLn
    $  show (hashes - numMonsters * count (== '#') monsterLiteral)
    ++ " (found "
    ++ show numMonsters
    ++ " monsters and "
    ++ show hashes
    ++ " total hashes"
    ++ ")"

  return ()

-- Counts how many monsters were found in a given string.
findMonsters :: Int -> String -> Int
findMonsters gridSize src = count
  (\(idx, _) -> idx `mod` totalSize < totalSize - monsterLen)
  matches
 where
  --  We'll use a regex to search for the monsters.
  matches =
    getAllMatches (dropNewlines src =~ monsterRegexWithLookahead) :: [ ( Int
        , Int
        )
      ]
  monsterRegexWithLookahead =
    head monsterRegex : ("(?=" ++ tail monsterRegex ++ ")") -- Use a lookahead to check for overlapping matches.
  monsterRegex =
    map (\x -> if x == ' ' then '.' else x) -- Spaces should match anything, #'s should match exactly.
      . reverse
      . dropWhile (== ' ') -- Trim spaces at the end of the regex.
      . reverse
      $ monsterString
  monsterString =
    concatMap (\x -> x ++ replicate (totalSize - monsterLen) ' ') -- Fill the literal with spaces up to the width of the grid.
      $ lines monsterLiteral
  monsterLen   = length . head . lines $ monsterLiteral -- Length of one line of monster.
  dropNewlines = concat . lines
  totalSize    = gridSize * (tileSize - 2)

-- Converts a grid of tile refs to a (printable) grid of chars.
pixeliseTiles :: TileMap -> V.Vector (V.Vector TileRef) -> [[Char]]
pixeliseTiles tm = concatMap mergeRow . V.toList
 where
  mergeRow row =
    map (map (bool '.' '#') . V.toList)
      . V.toList
      . foldr1 (V.zipWith (V.++))
      $ V.map tile row
  tile ref =
    transformTile ref . removeBorder . tileGrid . (tm M.!) . tileRefID $ ref

removeBorder :: TileGrid -> TileGrid
removeBorder = V.map (V.init . V.tail) . V.init . V.tail

-- Transforms a tile grid with rotation & flipping based on a TileRef.
transformTile :: TileRef -> TileGrid -> TileGrid
transformTile ref g = iterate rotateGrid g' !! tileRefRotation ref
  where g' = if tileRefFlipped ref then V.map V.reverse g else g

fuseTiles :: TileMap -> EdgeMap -> V.Vector (V.Vector TileRef)
fuseTiles tm em = V.ifoldl go V.empty blankGrid
 where
  go rowAcc r row = rowAcc `V.snoc` V.ifoldl go' V.empty row
   where
    go' colAcc c t = colAcc `V.snoc` tileRef
     where
      tileRef =
        if c == 0 && r == 0 then upperLeftRef else getNextTileRef mleft mup
      mleft = colAcc V.!? (j - 1)
      mup   = rowAcc V.!? (i - 1) >>= (V.!? j)
      i     = length rowAcc
      j     = length colAcc

  blankGrid    = V.replicate gridSize (V.replicate gridSize NullTileRef)
  gridSize     = sqrti $ M.size tm

  upperLeftRef = solveTile
    em
    (tm M.! upperLeftID)
    [UniqueEdgeConstraint, NoConstraint, NoConstraint, UniqueEdgeConstraint]
  upperLeftID = head corners

  getNextTileRef :: Maybe TileRef -> Maybe TileRef -> TileRef
  getNextTileRef mleft mup = solveTile em t constraints
   where
    -- Find next tile based on matching edge.
    t = tm M.! case mleft of
      Nothing -> case mup of
        Nothing -> error "left and up tile refs are both null"
        Just up -> getConnectableTile up South
      Just left -> getConnectableTile left East

    -- Returns the tile id of the tile with a matching edge to the specified ref and side.
    getConnectableTile ref side = if length tileIDs == 2
      then head $ tileIDs \\ [tileRefID ref]
      else error "Edge mismatch!?"
     where
      tileIDs = em M.! edge
      edge    = minimumEdgeID $ getEdgeFromTileRef ref side

    -- Construct the set of constraints for this tile.
    constraints =
      [ case mup of
        Nothing -> UniqueEdgeConstraint
        Just up ->
          MatchConstraint $ minimumEdgeID $ getEdgeFromTileRef up South
      , NoConstraint
      , NoConstraint
      , case mleft of
        Nothing -> UniqueEdgeConstraint
        Just left ->
          MatchConstraint $ minimumEdgeID $ getEdgeFromTileRef left East
      ]

  getEdgeFromTileRef :: TileRef -> TileSide -> EdgeID
  getEdgeFromTileRef ref side = getEdge (tm M.! tileRefID ref)
                                        (tileRefFlipped ref)
                                        side
                                        (tileRefRotation ref)

  corners = map (tileID . snd) . M.toList $ M.filter p tm -- Tiles with two unique edges are corners.
   where
    p        = (== 2) . count (isUnique . toEdgeID) . edges
    isUnique = (== 1) . countTilesWithSameEdge em

checkConstraints :: TileConstraint -> TileConstraint -> Bool
checkConstraints tcs tcs' = and $ zipWith check tcs tcs'
 where
  check NoConstraint        _                   = True
  check _                   NoConstraint        = True
  check (MatchConstraint a) (MatchConstraint b) = a == b
  check tc                  tc'                 = tc == tc'

solveTile :: EdgeMap -> Tile -> TileConstraint -> TileRef
solveTile em t@(Tile id _) tcs = TileRef { tileRefID       = id
                                         , tileRefFlipped  = flipped
                                         , tileRefRotation = rotation
                                         }
 where
  es          = edges t
  es'         = map (reverse . (es !!)) [0, 3, 2, 1] -- Flip along the vertical.
  constraints = map (edgeToConstraint . toEdgeID)
  edgeToConstraint e = case countTilesWithSameEdge em e of
    1 -> UniqueEdgeConstraint
    2 -> MatchConstraint $ minimumEdgeID e
    _ -> NoConstraint
  flipped =
    not $ any (checkConstraints tcs . (`rotate` constraints es)) [3, 2 .. 0] -- Descending since we want +ve = clockwise.
  rotation = head $ filter
    (checkConstraints tcs . (`rotate` constraints (if flipped then es' else es))
    )
    [3, 2 .. 0]
