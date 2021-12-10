import Data.List

type Case = [String]
type Soln = Int

type Stack a = [a]

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . lines

solve :: Case -> Soln
solve input = points !! (length points `div` 2)
  where
    statuses = zip input $ map (isNotCorrupt []) input
    incompleteLines = map fst $ filter snd statuses
    completionStrings = map (completionString []) incompleteLines
    points = sort $ map (calcPoints 0) completionStrings

getPoints :: Char -> Int
getPoints ')' = 1
getPoints ']' = 2
getPoints '}' = 3
getPoints '>' = 4
getPoints _ = error "Unknown character"

calcPoints :: Int -> String -> Int
calcPoints points [] = points
calcPoints points remaining = calcPoints updatedPoints (tail remaining)
    where
      nextCharacter = head remaining
      updatedPoints = points * 5 + getPoints nextCharacter

isNotCorrupt :: Stack Char -> String -> Bool
isNotCorrupt _ [] = True
isNotCorrupt stack ('(' : remaining) = isNotCorrupt (pushStack stack '(') remaining
isNotCorrupt stack ('{' : remaining) = isNotCorrupt (pushStack stack '{') remaining
isNotCorrupt stack ('[' : remaining) = isNotCorrupt (pushStack stack '[') remaining
isNotCorrupt stack ('<' : remaining) = isNotCorrupt (pushStack stack '<') remaining
isNotCorrupt stack (')' : remaining) = (peekStack stack == Just '(') && isNotCorrupt (popStack stack) remaining
isNotCorrupt stack ('}' : remaining) = (peekStack stack == Just '{') && isNotCorrupt (popStack stack) remaining
isNotCorrupt stack (']' : remaining) = (peekStack stack == Just '[') && isNotCorrupt (popStack stack) remaining
isNotCorrupt stack ('>' : remaining) = (peekStack stack == Just '<') && isNotCorrupt (popStack stack) remaining
isNotCorrupt _ _ = False

pushStack :: [a] -> a -> [a]
pushStack stack element = element : stack

peekStack :: [a] -> Maybe a
peekStack stack = if null stack then Nothing else Just (head stack)

popStack :: [a] -> [a]
popStack = tail

completionString :: Stack Char -> String -> Stack Char
completionString stack [] = stack
completionString stack ('(' : remaining) = completionString (pushStack stack ')') remaining
completionString stack ('{' : remaining) = completionString (pushStack stack '}') remaining
completionString stack ('[' : remaining) = completionString (pushStack stack ']') remaining
completionString stack ('<' : remaining) = completionString (pushStack stack '>') remaining
completionString stack (')' : remaining) = completionString (popStack stack) remaining
completionString stack ('}' : remaining) = completionString (popStack stack) remaining
completionString stack (']' : remaining) = completionString (popStack stack) remaining
completionString stack ('>' : remaining) = completionString (popStack stack) remaining
completionString _ _ = error "Encountered unknown character"
