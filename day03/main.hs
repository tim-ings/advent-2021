import Data.List
import Data.Char

type DiagnosticReport = [Int]

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . lines

solve :: [String] -> Int
solve bits = oxygenRating * co2Rating 
  where reports = map parseDiagnosticReport bits
        oxygenRating = decimal $ head $ calcOxygenRating reports 0
        co2Rating = decimal $ head $ calcCO2Rating reports 0

parseDiagnosticReport :: String -> DiagnosticReport
parseDiagnosticReport = map (read . (:""))

calcOxygenRating :: [DiagnosticReport] -> Int -> [DiagnosticReport]
calcOxygenRating [report] consideredPosition = [report]
calcOxygenRating reports consideredPosition = calcOxygenRating remainingReports (consideredPosition + 1)
  where
    (zerosCount, onesCount) = binaryFrequencies reports !! consideredPosition
    mostCommon = mostCommonAtPosition reports consideredPosition
    remainingReports = filter (\report -> report !! consideredPosition == mostCommon) reports

calcCO2Rating :: [DiagnosticReport] -> Int -> [DiagnosticReport]
calcCO2Rating [report] consideredPosition = [report]
calcCO2Rating reports consideredPosition = calcCO2Rating remainingReports (consideredPosition + 1)
  where
    (zerosCount, onesCount) = binaryFrequencies reports !! consideredPosition
    leastCommon = leastCommonAtPosition reports consideredPosition
    remainingReports = filter (\report -> report !! consideredPosition == leastCommon) reports

mostCommonAtPosition :: [DiagnosticReport] -> Int -> Int
mostCommonAtPosition reports consideredPosition
  | zerosCount > onesCount = 0
  | onesCount > zerosCount = 1
  | otherwise = 1
  where
    (zerosCount, onesCount) = binaryFrequencies reports !! consideredPosition

leastCommonAtPosition :: [DiagnosticReport] -> Int -> Int
leastCommonAtPosition reports consideredPosition
  | zerosCount < onesCount = 0
  | onesCount < zerosCount = 1
  | otherwise = 0
  where
    (zerosCount, onesCount) = binaryFrequencies reports !! consideredPosition

binaryFrequencies :: [DiagnosticReport] -> [(Int, Int)]
binaryFrequencies reports = zip zerosCount onesCount
  where transposedReports = transpose reports
        zerosCount = map (length . filter (== 0)) transposedReports
        onesCount = map (length . filter (== 1)) transposedReports

decimal :: [Int] -> Int
decimal bits = foldl' (\acc x -> acc * 2 + digitToInt x) 0 bitString
  where bitString = intercalate "" $ map show bits
