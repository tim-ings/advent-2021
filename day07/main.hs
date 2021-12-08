type Case = [Int]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve positions = bestCost
  where
    minPosition = minimum positions
    maxPosition = maximum positions
    costs = [costAt n positions | n <- [minPosition .. maxPosition]]
    bestCost = minimum costs

readCase :: String -> Case
readCase = map read . splitWith ',' . head . lines

splitWith :: Char -> String -> [String]
splitWith delimiter string = words [if c == delimiter then ' ' else c | c <- string]

costAt :: Int -> Case -> Int
costAt n positions = sum costs
  where
    distances = map (distanceBetween n) positions
    costs = map cost distances

distanceBetween :: Int -> Int -> Int
distanceBetween a b = abs $ a - b

cost :: Int -> Int
cost 0 = 0
cost 1 = 1
cost n = n + cost (n - 1)
