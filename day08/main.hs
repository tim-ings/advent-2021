type Case = ([String], [String])
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . map readCase . lines

solve :: [Case] -> Soln
solve cases = sum counts
  where
    outputs = map snd cases
    counts = [sum $ map countOnes outputs, sum $ map countFours outputs, sum $ map countSevens outputs, sum $ map countEights outputs]

readCase :: String -> Case
readCase rawCase = (words inputs, words $ tail outputs)
  where
    (inputs, outputs) = break (== '|') rawCase

countOnes :: [String] -> Int
countOnes = length . filter (\x -> length x == 2)

countFours :: [String] -> Int
countFours = length . filter (\x -> length x == 4)

countSevens :: [String] -> Int
countSevens = length . filter (\x -> length x == 3)

countEights :: [String] -> Int
countEights = length . filter (\x -> length x == 7)
