module Days.D12 where

import           Data.Char
import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import           Data.List
import           Data.List.Split
import           Utils


type Node = String
type Edge = (Node, Node)
data Graph = Graph
  { nodes :: [Node]
  , adjs  :: M.HashMap Node [Node]
  }
  deriving Show
data Path = Path
  { visited :: S.HashSet Node -- Set of visited nodes.
  , prev    :: Node -- Most recently visited node.
  , twice   :: Bool -- Whethr any small node has already been visited twice before.
  }
  deriving Show


parse :: String -> Graph
parse = lines .> foldr (splitOn "-" .> addEdge) (Graph [] M.empty) 
              .> finalise
 where
  addEdge [from, to] (Graph ns adjM) = Graph
    { nodes = from : to : ns
    , adjs  = adjM |> M.insertWith (++) to [from] 
                   |> M.insertWith (++) from [to]
    }
  addEdge _ _ = undefined
  finalise (Graph ns es) = Graph { nodes = nub ns, adjs = es }

part1 :: Graph -> Int
part1 g = spelunk g cond "start" "end"
  where cond path n = isBig n || n `notElem` visited path

part2 :: Graph -> Int
part2 g = spelunk g cond "start" "end"
 where
  cond path n =
    isBig n || (n /= "start" && (n `notElem` visited path || not (twice path)))

isBig :: Node -> Bool
isBig = all isUpper

isSmall :: Node -> Bool
isSmall = not . isBig

spelunk
  :: Graph -- Input graph.
  -> (Path -> Node -> Bool) -- Binary predicate to filter nodes to add to the path.
  -> Node -- Start node.
  -> Node -- Target node.
  -> Int -- Returns all paths from start to target.
spelunk (Graph _ adjM) cond s t = bfs
  [Path { visited = S.singleton s, prev = s, twice = False }]
  0
 where
  bfs [] cnt = cnt
  bfs q  cnt = bfs (concat newqs) (cnt + count (== True) finished)
   where
    bfsImpl path@Path { visited = v, prev = cur, twice = tw }
      | cur == t  = ([], True)
      | otherwise = (newNs |> map addNodeToPath, False)
     where
      addNodeToPath n = Path { visited = v |> S.insert n
                             , prev    = n
                             , twice   = tw || (isSmall n && n `elem` v)
                             }
      newNs = filter (cond path) (adjM M.! cur)
    (newqs, finished) = q |> map bfsImpl |> unzip

