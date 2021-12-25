import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isNothing, mapMaybe)

data Cucumber
  = South
  | East
  deriving (Show, Eq)
type Coord = (Int, Int)
type CucumberRegion = Map Coord Cucumber

type Case = (Int, Int, CucumberRegion)
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve (width, height, region) = succ $ length $ iterateToStable (step width height) region

showRegion :: Int -> Int -> CucumberRegion -> String
showRegion width height region = unlines $ map (showRegionLine width region) [0 .. height]

showRegionLine :: Int -> CucumberRegion -> Int -> String
showRegionLine width region y = map formatPoint points
  where
    points = zip [0 .. width] (repeat y)
    formatPoint = showCucumber . flip Map.lookup region

showCucumber :: Maybe Cucumber -> Char
showCucumber (Just South) = 'v'
showCucumber (Just East) = '>'
showCucumber Nothing = '.'

readCase :: String -> Case
readCase raw = (width - 1, height - 1, Map.fromList cucumbers)
  where
    rawLines = lines raw
    width = length $ head rawLines
    height = length rawLines
    cucumbers = concatMap readLine $ zip [0 .. height] rawLines

readLine :: (Int, String) -> [(Coord, Cucumber)]
readLine (y, rawLine) = mapMaybe readCucumber $ zip coords rawLine
  where
    coords = [(x, y) | x <- [0 .. length rawLine]]

readCucumber :: (Coord, Char) -> Maybe (Coord, Cucumber)
readCucumber (coord, '>') = Just (coord, East)
readCucumber (coord, 'v') = Just (coord, South)
readCucumber (coord, '.') = Nothing
readCucumber _ = error "Unknown symbol when reading cucumber"

step :: Int -> Int -> CucumberRegion -> CucumberRegion
step width height region = region''
  where
    eastMovers = Map.filter (== East) $ findMovingCucumbers width height region
    region' = moveCucumbers width height eastMovers region
    southMovers = Map.filter (== South) $ findMovingCucumbers width height region'
    region'' = moveCucumbers width height southMovers region'

iterateToStable :: Eq a => (a -> a) -> a -> [a]
iterateToStable fn initial = map snd $ takeWhile (uncurry (/=)) $ iterate (\(_, next) -> (next, fn next)) (initial, fn initial)

moveCucumbers :: Int -> Int -> CucumberRegion -> CucumberRegion -> CucumberRegion
moveCucumbers width height movers region = Map.union stationaryCucumbers movedCucumbers
  where
    movingPoints = Map.keysSet movers
    stationaryCucumbers = Map.filterWithKey (\coord _ -> Set.notMember coord movingPoints) region
    movedCucumbers = Map.mapKeys move movers
    move (x, y) =
      case Map.lookup (x, y) region of
        Just South -> (x, succWrap height y)
        Just East -> (succWrap width x, y)
        Nothing -> error "Failed to find moved cucumber in region"

findMovingCucumbers :: Int -> Int -> CucumberRegion -> CucumberRegion
findMovingCucumbers width height region = Map.filterWithKey (curry (canMove width height region)) region

canMove :: Int -> Int -> CucumberRegion -> (Coord, Cucumber) -> Bool
canMove width height region (coord, South) = isNothing $ Map.lookup (nextDestination width height coord South) region
canMove width height region (coord, East) = isNothing $ Map.lookup (nextDestination width height coord East) region

nextDestination :: Int -> Int -> Coord -> Cucumber -> Coord
nextDestination width height (x, y) South = (x, succWrap height y)
nextDestination width height (x, y) East = (succWrap width x, y)

succWrap :: Int -> Int -> Int
succWrap maxN n
  | n >= maxN = 0
  | otherwise = succ n
