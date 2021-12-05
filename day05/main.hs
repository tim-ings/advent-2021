import Data.List
import qualified Data.Map as DataMap

type Point = (Int, Int)
type Line = (Point, Point)
type CellMap = DataMap.Map Point Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . lines

solve :: [String] -> String
solve rawLines = show $ DataMap.size dangerousPoints
  where
    points = generatePoints $ map parseLine rawLines
    cells = populateCells DataMap.empty points
    dangerousPoints = DataMap.filter (> 1) cells

populateCells :: CellMap -> [Point] -> CellMap
populateCells cells [] = cells
populateCells cells points = populateCells updatedCells remainingPoints
  where
    updatedCells = DataMap.insertWith (+) (head points) 1 cells
    remainingPoints = tail points

generatePoints :: [Line] -> [Point]
generatePoints lines = flat $ map expandLine $ filter isValidLine lines

expandLine :: Line -> [Point]
expandLine ((x0, y0), (x1, y1)) = [(x, y) | x <- [startX .. endX], y <- [startY .. endY]]
  where
    (startX, endX) = sortPoint (x0, x1)
    (startY, endY) = sortPoint (y0, y1)

sortPoint :: Point -> Point
sortPoint (x, y) = (min x y, max x y)

isValidLine :: Line -> Bool
isValidLine line = isVertical line || isHorizontal line

isHorizontal :: Line -> Bool
isHorizontal ((x0, y0), (x1, y1)) = x0 == x1

isVertical :: Line -> Bool
isVertical ((x0, y0), (x1, y1)) = y0 == y1

-- Parses "0,9 -> 5,9"
parseLine :: String -> Line
parseLine rawLine = (a, b)
   where
    rawA : _ : rawB : _ = words rawLine
    a = parsePoint rawA
    b = parsePoint rawB

-- Parses "0,9"
parsePoint :: String -> Point
parsePoint rawPoint = (read rawX, read rawY)
  where
    rawX : rawY : _ = splitWith ',' rawPoint

splitWith :: Char -> String -> [String]
splitWith delimiter string = words [if c == delimiter then ' ' else c | c <- string]

flat :: [[a]] -> [a]
flat = foldl' (++) []
