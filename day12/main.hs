import Data.List.Split
import Data.Char
import qualified Data.Set as Set

data Cave
  = StartCave
  | EndCave
  | SmallCave String
  | LargeCave String
  deriving (Show, Eq, Ord)

type Edge a = (a, a)
type Graph a = [Edge a]
type CaveSystem = Graph Cave
type Case = CaveSystem
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve = length . findPaths [] StartCave

readCase :: String -> Case
readCase raw = map readLine $ lines raw

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
    frontier = unvisitedFrontier updatedPath $ getFrontier origin caveSystem
    findNextPath nextOrigin = findPaths updatedPath nextOrigin caveSystem

getFrontier :: Cave -> CaveSystem -> [Cave]
getFrontier origin = map (relatedCave origin) . filter (edgeContains origin)
  where
    edgeContains origin (a, b) = origin == a || origin == b
    relatedCave origin (a, b) = if origin == a then b else a

unvisitedFrontier :: [Cave] -> [Cave] -> [Cave]
unvisitedFrontier visited frontier = Set.toList $ Set.difference frontierSet multipleVisitsPermittedMask
  where
    multipleVisitsPermittedMask =
      if hasVisitedSmallCaveTwice visited then Set.filter (not . isLargeCave) $ Set.fromList visited
      else Set.filter (not . isLargeOrSmallCave) $ Set.fromList visited
    frontierSet = Set.fromList frontier

hasVisitedSmallCaveTwice :: [Cave] -> Bool
hasVisitedSmallCaveTwice visited = smallCavesCount > uniqueSmallCavesCount
  where
    smallCavesCount = length $ filter isSmallCave visited
    uniqueSmallCavesCount = Set.size $ Set.filter isSmallCave $ Set.fromList visited

isLargeCave :: Cave -> Bool
isLargeCave (LargeCave _) = True
isLargeCave _ = False

isSmallCave :: Cave -> Bool
isSmallCave (SmallCave _) = True
isSmallCave _ = False

isLargeOrSmallCave :: Cave -> Bool
isLargeOrSmallCave cave = isSmallCave cave || isLargeCave cave
