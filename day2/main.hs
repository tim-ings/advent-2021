data Direction
  = Forward
  | Up
  | Down
  | Unknown

type Instruction = (Direction, Int)

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . lines

solve :: [String] -> Int
solve rawInstructions = formatOutput location
  where directions = map (parseInstructionDirection . head) rawInstructions
        magnitudes = map parseInstructionMagnitude rawInstructions
        instructions = zip directions magnitudes
        location = foldl applyInstruction (0, 0, 0) instructions

formatOutput :: (Int, Int, Int) -> Int
formatOutput (_, depth, position) = depth * position

applyInstruction :: (Int, Int, Int) -> Instruction -> (Int, Int, Int)
applyInstruction (aim, depth, position) (Up, magnitude) = (aim - magnitude, depth, position)
applyInstruction (aim, depth, position) (Down, magnitude) = (aim + magnitude, depth, position)
applyInstruction (aim, depth, position) (Forward, magnitude) = (aim, depth + (aim * magnitude), position + magnitude)
applyInstruction instruction _ = instruction

parseInstructionDirection :: Char -> Direction
parseInstructionDirection 'f' = Forward
parseInstructionDirection 'u' = Up
parseInstructionDirection 'd' = Down
parseInstructionDirection _ = Unknown

parseInstructionMagnitude :: String -> Int
parseInstructionMagnitude = read . head . tail . words
