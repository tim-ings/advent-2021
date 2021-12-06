import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type SimulationState = Map Int Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . lines

solve :: [String] -> String
solve rawLines = show $ countFish result
  where
    state = parseInput rawLines
    result = simulateDays (256 + 1) state

parseInput :: [String] -> SimulationState
parseInput rawLine = state
  where
    ages = map read $ splitWith ',' $ head rawLine
    state = foldl' (\state age -> Map.insertWith (+) age 1 state) Map.empty ages

countFish :: SimulationState -> Int
countFish = Map.foldl' (+) 0

splitWith :: Char -> String -> [String]
splitWith delimiter string = words [if c == delimiter then ' ' else c | c <- string]

simulateDays :: Int -> SimulationState -> SimulationState
simulateDays 0 state = state
simulateDays n state = simulateDays (pred n) (simulate state)
  where
    simulate = simulateDayProgressed . simulateReproduction

simulateDayProgressed :: SimulationState -> SimulationState
simulateDayProgressed = Map.mapKeysWith (+) pred

simulateReproduction :: SimulationState -> SimulationState
simulateReproduction state = updateState state
  where
    reproducingCount = fromMaybe 0 $ Map.lookup (-1) state
    introduceChildren = Map.insertWith (+) 8 reproducingCount
    removeParents = Map.filterWithKey (\age _ -> age >= 0)
    reintroduceParents = Map.insertWith (+) 6 reproducingCount
    resetNewParents = removeParents . reintroduceParents
    updateState = introduceChildren . resetNewParents
