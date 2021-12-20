import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (digitToInt)

type Pixel = Int
type Algorithm = [Pixel]
type Coord = (Int, Int)
type Image = Map Coord Pixel

type Case = (Algorithm, Image)
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve (algorithm, image) = length litPixels
  where
    enhancedImage = applyEnhance 2 algorithm image
    litPixels = Map.filter (== 1) enhancedImage

applyEnhance :: Int -> Algorithm -> Image -> Image
applyEnhance 0 _ image = image
applyEnhance n algorithm image = applyEnhance (pred n) algorithm (enhanceImage (defaultPixel n) algorithm image)
  where
    shouldFlipPixel = head algorithm == 1 && last algorithm == 0
    flipPixel n = if even n then 0 else 1
    defaultPixel n = if shouldFlipPixel then flipPixel n else 0

showImage :: Image -> String
showImage image = unlines rows
  where
    (minX, minY, maxX, maxY) = imageBounds image
    showRow row = [(showPixel . (`Map.lookup` image)) (x, row) | x <- [minX - 1 .. maxX + 1]]
    rows = [showRow y | y <- [minY - 1 .. maxY + 1]]

showPixel :: Maybe Int -> Char
showPixel (Just 1) = '#'
showPixel _ = '.'

readCase :: String -> Case
readCase rawInput = (algorithm, image)
  where
    rawLines = lines rawInput
    algorithm = map readPixel (head rawLines)
    rawImage = drop 2 rawLines
    width = length (head rawImage)
    height = length rawImage
    image = Map.fromList $ map (readPixelAt rawImage) (generatePoints (0, 0, width, height))

readPixelAt :: [[Char]] -> Coord -> (Coord, Pixel)
readPixelAt rawImage (x, y) = ((x, y), readPixel (rawImage !! y !! x))

readPixel :: Char -> Pixel
readPixel colour = case colour of
  '#' -> 1
  '.' -> 0
  _ -> error "Unknown light level in input image"

readAlgorithm :: [String] -> String
readAlgorithm = head

readInputImage :: [String] -> [String]
readInputImage = drop 2

imageBounds :: Image -> (Int, Int, Int, Int)
imageBounds image = (minimum xs, minimum ys, maximum xs, maximum ys)
  where
    xs = map fst $ Map.keys image
    ys = map snd $ Map.keys image

enhanceImage :: Pixel -> Algorithm -> Image -> Image
enhanceImage defaultPixel algorithm image = enhancedImage
  where
    (minX, minY, maxX, maxY) = imageBounds image
    consideredPoints = generatePoints (minX - 1, minY - 1, maxY + 2, maxY + 2)
    enhancedImage = Map.fromList $ map (enhancePixel defaultPixel algorithm image) consideredPoints

enhancePixel :: Pixel -> Algorithm -> Image -> Coord -> (Coord, Pixel)
enhancePixel defaultPixel algorithm image coord = (coord, algorithm !! algorithmIndex)
  where
    considered = consideredPixels coord
    consideredValues = map (\neighbour -> Map.findWithDefault defaultPixel neighbour image) considered
    algorithmIndex = binToInt consideredValues
    consideredPixels (x, y) =
      [ (x - 1, y - 1) -- top left
      , (x    , y - 1) -- top middle
      , (x + 1, y - 1) -- top right
      , (x - 1, y    ) -- left
      , (x    , y    ) -- middle
      , (x + 1, y    ) -- right
      , (x - 1, y + 1) -- bottom left
      , (x    , y + 1) -- bottom middle
      , (x + 1, y + 1) -- bottom right
      ]

binToInt :: [Int] -> Int
binToInt = sum . zipWith (*) (iterate (2*) 1) . reverse

generatePoints :: (Int, Int, Int, Int) -> [Coord]
generatePoints (x, y, width, height) = (,) <$> [x .. width - 1] <*> [y .. height - 1]
