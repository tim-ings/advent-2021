import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Char (digitToInt)
import Data.Graph.AStar (aStar)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)

type Point = (Int, Int)
type Risk = Int
type Node = (Point, Risk)
type Graph = Map Node (HashSet Node)

type Case = (Graph, Node, Node)
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve (graph, start, goal) = cost path
  where
    path = shortestPath graph start goal
    cost = sum . map snd

readCase :: String -> Case
readCase rawInput = (graph, start, goal)
  where
    grid = expandGrid $ map (map digitToInt) (lines rawInput)
    width = length (head grid)
    height = length grid
    points = (,) <$> [0 .. width - 1] <*> [0 .. height - 1]
    buildRiskNode point = (point, riskAt grid point)
    adjacencies = map (\point -> (buildRiskNode point, HashSet.map buildRiskNode $ neighbouringPoints grid point)) points
    graph = Map.fromList adjacencies
    start = buildRiskNode (0, 0)
    goal = buildRiskNode (width - 1, height - 1)

succWrap :: Risk -> Risk
succWrap n
  | n >= 9 = 1
  | otherwise = succ n

expandGrid :: [[Risk]] -> [[Risk]]
expandGrid grid = tallGrid
  where
    wideGrid = map (expandRight 5 []) grid
    tallGrid = expandDown 5 [] wideGrid

expandRight :: Int -> [Risk] -> [Risk] -> [Risk]
expandRight 0 expanded _ = expanded
expandRight n [] row = expandRight (pred n) row row
expandRight n expanded row = expandRight (pred n) (expanded ++ map succWrap lastExpansion) row
  where
    lastExpansion = reverse $ take (length row) $ reverse expanded

expandDown :: Int -> [[Risk]] -> [[Risk]] -> [[Risk]]
expandDown 0 expanded _ = expanded
expandDown n [] grid = expandDown (pred n) grid grid
expandDown n expanded grid = expandDown (pred n) (expanded ++ map (map succWrap) lastExpansion) grid
  where
    lastExpansion = reverse $ take (length grid) $ reverse expanded

riskAt :: [[Risk]] -> Point -> Risk
riskAt grid (x, y) = (grid !! x) !! y

neighbouringPoints :: [[Risk]] -> Point -> HashSet Point
neighbouringPoints grid (x, y) = HashSet.fromList $ filter (isValidNeighbour grid) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isValidNeighbour :: [[Risk]] -> Point -> Bool 
isValidNeighbour grid (x, y) = x >= 0 && x < width && y > 0 && y < height
  where
    width = length (head grid)
    height = length grid

shortestPath :: Graph -> Node -> Node -> [Node]
shortestPath graph start goal = fromMaybe (error "No path found") $ aStar graph' distance heuristic goal' start
  where
    graph' k = Map.findWithDefault HashSet.empty k graph
    distance a b = snd b
    heuristic = euclideanDistance goal
    goal' = (==) goal

euclideanDistance :: Node -> Node -> Risk
euclideanDistance ((x1, y1), _) ((x2, y2), _) = round $ sqrt (dxSq + dySq)
  where
    dxSq = fromIntegral (x1 - x2) ** 2
    dySq = fromIntegral (y1 - y2) ** 2

manhattanDistance :: Node -> Node -> Risk
manhattanDistance ((x1, y1), _) ((x2, y2), _) = abs (x2 - x1) + abs (y2 - y1)
