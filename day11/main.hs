{-# LANGUAGE TupleSections #-}
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Char (digitToInt)

type Point = (Int, Int)
type Grid = Map Point Int
type Case = Grid
type Soln = Int

width = 10 :: Int
height = 10 :: Int
points = (,) <$> [0 .. width - 1] <*> [0 .. height - 1]

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve grid = snd $ applySteps 100 (grid, 0)

-- read

readCase :: String -> Case
readCase raw = Map.fromList $ map (\(i, j) -> ((i, j), values !! i !! j)) points
  where
    values = map (map digitToInt) $ lines raw

-- step

applySteps :: Int -> (Grid, Int) -> (Grid, Int)
applySteps 0 (grid, flashedCount) = (grid, flashedCount)
applySteps n (grid, flashedCount) = applySteps (pred n) $ step (grid, flashedCount)

step :: (Grid, Int) -> (Grid, Int)
step (grid, flashedCount) = (updatedGrid, flashedCount + pointsFlashed)
  where
    (updatedGrid, pointsFlashed) = flashGrid 0 $ incrementGrid grid

incrementGrid :: Grid -> Grid
incrementGrid = Map.map succ

-- flash

flashGrid :: Int -> Grid -> (Grid, Int)
flashGrid flashedCount grid =
  if morePointsToFlash then flashGrid updatedFlashedCount flashedGrid
  else (resetPoints (findLowEnergyEnergyPoints flashedGrid) flashedGrid, updatedFlashedCount)
  where
    pointsToFlash = findHighEnergyPoints grid
    flashedGrid = taintPoints pointsToFlash $ applyFlashes pointsToFlash grid
    morePointsToFlash = not $ null $ findHighEnergyPoints flashedGrid
    updatedFlashedCount = flashedCount + length pointsToFlash

findHighEnergyPoints :: Grid -> [Point]
findHighEnergyPoints = map fst . Map.toList . Map.filter (> 9)

findLowEnergyEnergyPoints :: Grid -> [Point]
findLowEnergyEnergyPoints = map fst . Map.toList . Map.filter (< 0)

applyFlashes :: [Point] -> Grid -> Grid
applyFlashes flashPoints grid = foldl' (\acc point -> Map.insertWith (+) point 1 acc) grid $ concatMap neighbours flashPoints

-- reset flashed

setPoints :: Int -> [Point] -> Grid -> Grid
setPoints value points grid = Map.unionWith (\_ _ -> value) grid $ Map.fromList $ map (,0) points

taintPoints :: [Point] -> Grid -> Grid
taintPoints = setPoints (-999999999999)

resetPoints :: [Point] -> Grid -> Grid
resetPoints = setPoints 0

-- neighbours

neighbours :: Point -> [Point]
neighbours = filter validNeighbour . neighbouringPoints
  where
    neighbouringPoints (i, j) = [(i - 1, j - 1), (i - 1, j), (i - 1, j + 1), (i, j - 1), (i, j + 1), (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)]
    validNeighbour (i, j) = i >= 0 && j >= 0 && i < height && j < width
