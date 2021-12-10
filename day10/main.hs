type Case = [String]
type Soln = Int

type Stack a = [a]

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . lines

solve :: Case -> Soln
solve input = sum points
  where
    statuses = zip input $ map (isNotCorrupt [] 0) input
    corruptLines = filter (not . fst . snd) statuses
    corruptChars = map (\(line, (_, position)) -> line !! position) corruptLines
    points = map getPoints corruptChars

getPoints :: Char -> Int
getPoints ')' = 3
getPoints ']' = 57
getPoints '}' = 1197
getPoints '>' = 25137
getPoints _ = 0

isNotCorrupt :: Stack Char -> Int -> String -> (Bool, Int)
isNotCorrupt _ position [] = (True, position)
isNotCorrupt stack position ('(' : remaining) = isNotCorrupt (pushStack stack '(') (succ position) remaining
isNotCorrupt stack position ('{' : remaining) = isNotCorrupt (pushStack stack '{') (succ position) remaining
isNotCorrupt stack position ('[' : remaining) = isNotCorrupt (pushStack stack '[') (succ position) remaining
isNotCorrupt stack position ('<' : remaining) = isNotCorrupt (pushStack stack '<') (succ position) remaining
isNotCorrupt stack position (')' : remaining) = if peekStack stack == Just '(' then isNotCorrupt (popStack stack) (succ position) remaining else (False, position)
isNotCorrupt stack position ('}' : remaining) = if peekStack stack == Just '{' then isNotCorrupt (popStack stack) (succ position) remaining else (False, position)
isNotCorrupt stack position (']' : remaining) = if peekStack stack == Just '[' then isNotCorrupt (popStack stack) (succ position) remaining else (False, position)
isNotCorrupt stack position ('>' : remaining) = if peekStack stack == Just '<' then isNotCorrupt (popStack stack) (succ position) remaining else (False, position)
isNotCorrupt _ position _ = (False, position)

pushStack :: [a] -> a -> [a]
pushStack stack element = element : stack

peekStack :: [a] -> Maybe a
peekStack stack = if null stack then Nothing else Just (head stack)

popStack :: [a] -> [a]
popStack = tail
