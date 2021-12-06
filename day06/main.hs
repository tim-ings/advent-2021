import Data.List
import Debug.Trace (traceShow)

type LanternfishAge = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . lines

solve :: [String] -> String
solve rawLines = show $ length result
  where
    fishAges = parseFishAges rawLines
    result = simulateDays 256 fishAges

parseFishAges :: [String] -> [LanternfishAge]
parseFishAges = map read . splitWith ',' . head

splitWith :: Char -> String -> [String]
splitWith delimiter string = words [if c == delimiter then ' ' else c | c <- string]

simulateDays :: Int -> [LanternfishAge] -> [LanternfishAge]
simulateDays 0 todayAges = todayAges
simulateDays n todayAges = simulateDays (pred n) (tomorrowCycledAges ++ newFish)
  where
    tomorrowAges = map pred todayAges
    newFishCount = length $ filter (< 0) tomorrowAges
    tomorrowCycledAges = map clampAgeCycle tomorrowAges
    newFish = replicate newFishCount 8

clampAgeCycle :: LanternfishAge -> LanternfishAge
clampAgeCycle age = if age < 0 then 6 else age
