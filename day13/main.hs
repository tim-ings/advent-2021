import Data.List
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

data FoldLine
  = VerticalLine Int
  | HorizontalLine Int
  deriving (Show, Eq, Ord)

type Coordinate = (Int, Int)

type Case = ([FoldLine], [Coordinate])
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve (foldLines, coordinates) = Set.size foldedPaper
  where
    firstFoldLine = head foldLines
    foldedPaper = foldPaper firstFoldLine $ Set.fromList coordinates

readCase :: String -> Case
readCase raw = foldl' readLine ([], []) (lines raw)

readLine :: Case -> String -> Case
readLine (foldLines, coordinates) ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'y':'=': value) = (foldLines ++ [HorizontalLine $ read value], coordinates)
readLine (foldLines, coordinates) ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'x':'=': value) = (foldLines ++ [VerticalLine $ read value], coordinates)
readLine (foldLines, coordinates) "" = (foldLines, coordinates)
readLine (foldLines, coordinates) rawLine = (foldLines, coordinates ++ [(read x, read y)])
  where
    [x, y] = splitOn "," rawLine

foldPaper :: FoldLine -> Set Coordinate -> Set Coordinate
foldPaper foldLine = Set.map (foldCoordinate foldLine)

foldCoordinate :: FoldLine -> Coordinate -> Coordinate
foldCoordinate (VerticalLine lineX) (x, y) = (lineX - abs (x - lineX), y)
foldCoordinate (HorizontalLine lineY) (x, y) = (x, lineY - abs (y - lineY))
