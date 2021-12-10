import Data.DiGraph (DiGraph)
import qualified Data.DiGraph as DiGraph
import qualified Data.HashSet as HashSet
import Data.List
import Debug.Trace (traceShow)

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
solve heightMap = sum $ map riskLevel lowPoints
  where
    graph = buildGraph heightMap
    nodes = HashSet.toList $ DiGraph.vertices graph
    lowPoints = filter (isLowPoint graph) nodes

isLowPoint :: HeightGraph -> HeightNode -> Bool
isLowPoint graph node = all (\(_, adjValue) -> adjValue > nodeValue) adjacencies
  where
    (_, nodeValue) = node
    adjacencies = HashSet.toList $ DiGraph.adjacents node graph

riskLevel :: HeightNode -> Int
riskLevel (_, value) = value + 1

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
