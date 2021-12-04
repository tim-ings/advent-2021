import Data.List

type Board = [[Int]]

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . lines

solve :: [String] -> String
solve rawLines = show $ calcOutput winningBoard draws drawCount
  where
    cleanedRawLines = filter (/= "") rawLines
    (remainingLines, draws) = parseDraws cleanedRawLines
    (_, boards) = parseAllBoards remainingLines []
    (winningBoard, drawCount) = incrementalSolve boards draws 1

calcOutput :: Board -> [Int] -> Int -> Int
calcOutput board draws drawCount = unmarkedSum * finalDraw
  where
    unmarkedSum = sumUnmarked board (headN draws [] drawCount)
    finalDraw = draws !! (drawCount - 2)

incrementalSolve :: [Board] -> [Int] -> Int -> (Board, Int)
incrementalSolve boards draws n =
  case winningBoard of
    Just winningBoard -> (winningBoard, n)
    Nothing -> incrementalSolve boards draws (n + 1)
  where
    winningBoard = find (isWinningBoard (headN draws [] n)) boards

parseDraws :: [String] -> ([String], [Int])
parseDraws rawLines = (remainingLines, draws)
  where
    (remainingLines, [line]) = extractLines rawLines [] 1
    draws = map read $ words [if c == ',' then ' ' else c | c <- line]

parseBoard :: [String] -> Board -> ([String], Board)
parseBoard [] board = ([], board)
parseBoard rawLines board =
  if length board == 5 then (rawLines, board)
  else parseBoard remainingLines (board ++ [nextRow]) 
  where
    nextRow = map read $ words $ head rawLines
    remainingLines = tail rawLines

parseAllBoards :: [String] -> [Board] -> ([String], [Board])
parseAllBoards [] boards = ([], boards)
parseAllBoards rawLines parsedBoards = parseAllBoards remainingLines (parsedBoards ++ [board])
  where
    (remainingLines, board) = parseBoard rawLines []

hasWinningRow :: Board -> [Int] -> Bool 
hasWinningRow [] _ = False
hasWinningRow board draws = length intersection >= 5 || hasWinningRow (tail board) draws
  where
    intersection = head board `intersect` draws
    isWinner = length intersection == 5

isWinningBoard :: [Int] -> Board -> Bool
isWinningBoard draws board = hasWinningRow board draws || hasWinningRow (transpose board) draws

sumUnmarked :: Board -> [Int] -> Int
sumUnmarked board marked = sum compliment
  where
    compliment = filter (`notElem` marked) (flat board)

extractLines :: [String] -> [String] -> Int -> ([String], [String])
extractLines raw extracted 0 = (raw, extracted)
extractLines raw extracted n = extractLines (tail raw) (extracted ++ [head raw]) (n - 1)

headN :: [a] -> [a] -> Int -> [a]
headN [] extracted _ = extracted
headN remaining extracted 1 = extracted
headN remaining extracted n = headN (tail remaining) (extracted ++ [head remaining]) (n - 1)

flat :: [[Int]] -> [Int]
flat = foldl' (++) []
