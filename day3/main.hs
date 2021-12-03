import Data.List
import Debug.Trace
import Data.Char

type DiagnosticReport = [Int]

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . lines

solve :: [String] -> String
solve bits = show $ gamma * epsilon
  where reports = map parseDiagnosticReport bits
        gamma = calcGamma reports
        epsilon = calcEpsilon reports

parseDiagnosticReport :: String -> DiagnosticReport
parseDiagnosticReport = map (read . (:""))

calcGamma :: [DiagnosticReport] -> Int
calcGamma reports = decimal $ map mostCommon zipped
  where
    transposedReports = trace (show reports) $ transpose reports
    oneFrequencies = map countOnes transposedReports
    zeroFrequencies = map countZeroes transposedReports
    zipped = zip zeroFrequencies oneFrequencies

calcEpsilon :: [DiagnosticReport] -> Int
calcEpsilon reports = decimal $ map leastCommon zipped
  where
    transposedReports = trace (show reports) $ transpose reports
    oneFrequencies = map countOnes transposedReports
    zeroFrequencies = map countZeroes transposedReports
    zipped = zip zeroFrequencies oneFrequencies

mostCommon :: (Int, Int) -> Int
mostCommon (a, b)
  | a > b = 0
  | otherwise = 1

leastCommon :: (Int, Int) -> Int
leastCommon (a, b)
  | a < b = 0
  | otherwise = 1

countOnes :: [Int] -> Int
countOnes = length . filter (== 1)

countZeroes :: [Int] -> Int
countZeroes = length . filter (== 0)

decimal :: [Int] -> Int
decimal bits = foldl' (\acc x -> acc * 2 + digitToInt x) 0 bitString
  where bitString = intercalate "" $ map show bits
