type Window = (Int, Int, Int)

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readInput

readInput :: String -> [Int]
readInput = map read . lines

solve :: [Int] -> Int
solve readings = length $ filter id $ zipWith (<) measurements $ tail measurements
  where windows = generateWindows readings
        measurements = map windowMeasurement windows

generateWindows :: [Int] -> [Window]
generateWindows readings = zip3 readings (tail readings) (tail $ tail readings)

windowMeasurement :: Window -> Int
windowMeasurement (a, b, c) = a + b + c
