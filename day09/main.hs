import Data.DiGraph (DiGraph)
import qualified Data.DiGraph as DiGraph
import qualified Data.HashSet as HashSet
import Data.List
import Data.Maybe (isJust, fromMaybe)
import Debug.Trace (traceShow)
import Data.Set (Set)
import qualified Data.Set as Set

type Case = [[Int]]
type Soln = Int

type Point = (Int, Int) -- (i, j)
type HeightNode = (Point, Int) -- ((i, j), value)
type HeightGraph = DiGraph HeightNode

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

readCase :: String -> Case
readCase = map (map readHeight) . lines

readHeight :: Char -> Int
readHeight ch = read [ch]

solve :: Case -> Soln
solve heightMap = product topBasinSizes
  where
    graph = pruneBasinBoundaries $ buildGraph heightMap
    shortestPathCache = DiGraph.shortestPathCache graph
    basins = findBasins heightMap graph shortestPathCache
    uniqueBasins = Set.toList $ Set.fromList basins
    basinSizes = map length uniqueBasins
    topBasinSizes = take 3 $ reverse $ sort basinSizes

findBasins :: Case -> HeightGraph -> DiGraph.ShortestPathCache HeightNode -> [[HeightNode]]
findBasins heightMap graph shortestPathCache = basins
  where
    gridWidth = length heightMap
    gridHeight = length $ head heightMap
    lowPoints = filter (isLowPoint graph) $ HashSet.toList $ DiGraph.vertices graph
    basins = sort $ map (basinAt graph shortestPathCache) lowPoints

isLowPoint :: HeightGraph -> HeightNode -> Bool
isLowPoint graph node = all (\(_, adjValue) -> adjValue > nodeValue) adjacencies
  where
    (_, nodeValue) = node
    adjacencies = HashSet.toList $ DiGraph.adjacents node graph

toNode :: Case -> Point -> HeightNode
toNode heightMap point = (point, heightAt heightMap point)

basinAt :: HeightGraph -> DiGraph.ShortestPathCache HeightNode -> HeightNode -> [HeightNode]
basinAt graph shortestPathCache startNode = map fst justDistances
  where
    vertices = HashSet.toList $ DiGraph.vertices graph
    distances = map (\node -> (node, DiGraph.distance_ startNode node shortestPathCache)) vertices
    justDistances = filter (\(_, distance) -> isJust distance) distances

pruneBasinBoundaries :: HeightGraph -> HeightGraph
pruneBasinBoundaries graph = DiGraph.fromEdges $ HashSet.filter (not . connectsBasinBoundary) $ DiGraph.edges graph

connectsBasinBoundary :: DiGraph.DiEdge HeightNode -> Bool
connectsBasinBoundary (a, b) = isBoundaryVertex a || isBoundaryVertex b

isBoundaryVertex :: HeightNode -> Bool
isBoundaryVertex (_, value) = value == 9

buildGraph :: Case -> HeightGraph
buildGraph heights = DiGraph.fromList adjacencies
  where
    gridWidth = length heights
    gridHeight = length $ head heights
    points = (,) <$> [0 .. gridWidth - 1] <*> [0 .. gridHeight - 1]
    adjacencies = map (calcAdjacencies heights) points

calcAdjacencies :: Case -> Point -> (HeightNode, [HeightNode])
calcAdjacencies heightMap point = (node, neighbouringNodes)
  where
    node = (point, heightAt heightMap point)
    gridWidth = length heightMap
    gridHeight = length $ head heightMap
    neighbouringPoints = filter (inBounds gridWidth gridHeight) (neighbours point)
    neighbouringNodes = map (\neighbouringPoint -> (neighbouringPoint, heightAt heightMap neighbouringPoint)) neighbouringPoints

heightAt :: Case -> Point -> Int
heightAt heightMap (i, j) = heightMap !! i !! j

neighbours :: Point -> [Point]
neighbours (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

inBounds :: Int -> Int -> Point -> Bool
inBounds maxI maxJ (i, j) = i >= 0 && j >= 0 && i < maxI && j < maxJ
