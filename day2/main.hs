import Distribution.Simple.InstallDirs (InstallDirs(InstallDirs))

data Direction
  = Forward
  | Backward
  | Up
  | Down
  | Unknown

type Instruction = (Direction, Int)

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . lines

solve :: [String] -> Int
solve rawInstructions = depth * position
  where directions = map (parseInstructionDirection . head) rawInstructions
        magnitudes = map parseInstructionMagnitude rawInstructions
        instructions = zip directions magnitudes
        depth = foldl applyDepth 0 instructions
        position = foldl applyPosition 0 instructions

applyDepth :: Int -> Instruction -> Int
applyDepth depth (Up, magnitude) = depth - magnitude
applyDepth depth (Down, magnitude) = depth + magnitude
applyDepth depth (_, magnitude) = depth

applyPosition :: Int -> Instruction -> Int
applyPosition position (Forward, magnitude) = position + magnitude
applyPosition position (Backward, magnitude) = position - magnitude
applyPosition position (_, magnitude) = position

parseInstructionDirection :: Char -> Direction
parseInstructionDirection 'f' = Forward
parseInstructionDirection 'b' = Backward
parseInstructionDirection 'u' = Up
parseInstructionDirection 'd' = Down
parseInstructionDirection _ = Unknown

parseInstructionMagnitude :: String -> Int
parseInstructionMagnitude = read . head . tail . words
