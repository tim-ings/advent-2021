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
type Soln = Set Coordinate

main :: IO ()
main = interact handle

handle :: String -> String
handle = showPaper . solve . readCase

solve :: Case -> Soln
solve (foldLines, coordinates) = foldl' foldPaper (Set.fromList coordinates) foldLines

showPaper :: Set Coordinate -> String
showPaper coordinates = unlines [[showSymbol x y | x <- [minimum xs .. maximum xs]] | y <- [minimum ys .. maximum ys]]
  where
    showSymbol x y = if Set.member (x, y) coordinates then '#' else '.'
    elements = Set.elems coordinates
    xs = map fst elements
    ys = map snd elements

readCase :: String -> Case
readCase raw = foldl' readLine ([], []) (lines raw)

readLine :: Case -> String -> Case
readLine (foldLines, coordinates) ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'y':'=': value) = (foldLines ++ [HorizontalLine $ read value], coordinates)
readLine (foldLines, coordinates) ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'x':'=': value) = (foldLines ++ [VerticalLine $ read value], coordinates)
readLine (foldLines, coordinates) "" = (foldLines, coordinates)
readLine (foldLines, coordinates) rawLine = (foldLines, coordinates ++ [(read x, read y)])
  where
    [x, y] = splitOn "," rawLine

foldPaper :: Set Coordinate -> FoldLine -> Set Coordinate
foldPaper coordinates foldLine = Set.map (foldCoordinate foldLine) coordinates

foldCoordinate :: FoldLine -> Coordinate -> Coordinate
foldCoordinate (VerticalLine lineX) (x, y) = (lineX - abs (x - lineX), y)
foldCoordinate (HorizontalLine lineY) (x, y) = (x, lineY - abs (y - lineY))
