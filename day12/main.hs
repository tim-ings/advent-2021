{-# LANGUAGE DeriveGeneric #-}
import Data.List
import Data.List.Split
import Data.Char
import Data.Hashable
import qualified Algebra.Graph.Undirected as Undirected
import qualified Data.Set as Set
import GHC.Generics

data Cave
  = StartCave
  | EndCave
  | SmallCave String
  | LargeCave String
  deriving (Show, Eq, Generic, Ord)

instance Hashable Cave where
  hash StartCave = hash "start"
  hash EndCave = hash "end"
  hash (SmallCave id) = hash id
  hash (LargeCave id) = hash id

type CaveSystem = Undirected.Graph Cave

type Case = CaveSystem
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve = length . findPaths [] StartCave

readCase :: String -> Case
readCase raw = Undirected.edges edges
  where
    edges = map readLine $ lines raw

readLine :: String -> (Cave, Cave)
readLine line = (sourceCave, destCave)
  where
    [sourceCave, destCave] = map parseCave $ splitWhen (== '-') line

parseCave :: String -> Cave
parseCave "start" = StartCave
parseCave "end" = EndCave
parseCave id = if map toUpper id == id then LargeCave id else SmallCave id

findPaths :: [Cave] -> Cave -> CaveSystem -> [[Cave]]
findPaths path origin caveSystem
  | hasPathReachedEnd updatedPath = [updatedPath]
  | isDeadEnd frontier = []
  | otherwise = concatMap findNextPath frontier
  where
    isDeadEnd = null
    hasPathReachedEnd = Set.member EndCave . Set.fromList
    updatedPath = path ++ [origin]
    frontier = unvisitedFrontier path $ getFrontier origin caveSystem
    findNextPath nextOrigin = findPaths updatedPath nextOrigin caveSystem

getFrontier :: Cave -> CaveSystem -> [Cave]
getFrontier origin = map (relatedCave origin) . filter (edgeContains origin) . Undirected.edgeList
  where
    edgeContains origin (a, b) = origin == a || origin == b
    relatedCave origin (a, b) = if origin == a then b else a

unvisitedFrontier :: [Cave] -> [Cave] -> [Cave]
unvisitedFrontier visited frontier = Set.toList $ Set.difference frontierSet visitedSet
  where
    visitedSet = Set.filter (not . multipleVisitsPermitted) $ Set.fromList visited
    frontierSet = Set.fromList frontier

multipleVisitsPermitted :: Cave -> Bool
multipleVisitsPermitted (LargeCave _) = True
multipleVisitsPermitted _ = False
