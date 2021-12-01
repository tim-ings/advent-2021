main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readInput

readInput :: String -> [Int]
readInput = map read . lines

solve :: [Int] -> Int
solve readings = length $ filter id $ zipWith (<) readings $ tail readings
