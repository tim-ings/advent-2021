import Data.List
import Data.Map (Map)
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Number as Parsec
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Debug.Trace (traceShow)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, fromLeft, fromRight)

data Operand
  = Literal Int
  | Variable Char
  deriving (Show, Eq)

data Instruction
  = Inp Operand
  | Add Operand Operand
  | Mul Operand Operand
  | Div Operand Operand
  | Mod Operand Operand
  | Eql Operand Operand
  deriving (Show, Eq)

type Memory = Map Char Int
type AluState = ([Int], [Instruction], Memory)

type Case = [Instruction]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve instructions = runMonad instructions [1,3,5,7,9,2,4,6,8,9,9,9,9,9]

runMonad :: [Instruction] -> [Int] -> Int
runMonad instructions stdin = fromMaybe (error "Instructions did not leave valid indicator") (Map.lookup 'z' memory)
  where
    (_, _, memory) = executeInstructions (stdin, instructions, Map.empty)

readCase :: String -> Case
readCase raw = case Parsec.parse parseInstruction "" raw of
  Right v -> v
  Left e -> error $ show e

parseInstruction :: Parser [Instruction]
parseInstruction = Parsec.many $ Parsec.choice $ map Parsec.try [parseInp, parseAdd, parseMul, parseDiv, parseMod, parseEql]
  where
    parseInp = do
      Parsec.string "inp "
      lhs <- parseOperand
      Parsec.endOfLine
      return $ Inp lhs
    parseAdd = do
      Parsec.string "add "
      (lhs, rhs) <- parseBinaryOperands
      Parsec.endOfLine
      return $ Add lhs rhs
    parseMul = do
      Parsec.string "mul "
      (lhs, rhs) <- parseBinaryOperands
      Parsec.endOfLine
      return $ Mul lhs rhs
    parseDiv = do
      Parsec.string "div "
      (lhs, rhs) <- parseBinaryOperands
      Parsec.endOfLine
      return $ Div lhs rhs
    parseMod = do
      Parsec.string "mod "
      (lhs, rhs) <- parseBinaryOperands
      Parsec.endOfLine
      return $ Mod lhs rhs
    parseEql = do
      Parsec.string "eql "
      (lhs, rhs) <- parseBinaryOperands
      Parsec.endOfLine
      return $ Eql lhs rhs

parseOperand :: Parser Operand
parseOperand = Parsec.choice [Parsec.try parseVariable, Parsec.try parseLiteral]
  where
    parseVariable = Variable <$> Parsec.oneOf "wxyz"
    parseLiteral = Literal <$> parseInt

parseInt :: Parser Int
parseInt = do
  sign <- Parsec.sign
  sign . read <$> Parsec.many1 Parsec.digit

parseBinaryOperands :: Parser (Operand, Operand)
parseBinaryOperands = do
  lhs <- parseOperand
  Parsec.space
  rhs <- parseOperand
  return (lhs, rhs)

executeInstructions :: AluState -> AluState
executeInstructions (stdin, [], memory) = (stdin, [], memory)
executeInstructions (stdin, instructions, memory) = executeInstructions (stdin', instructions', memory')
  where
    instructions' = tail instructions
    (stdin', memory') = executeInstruction (stdin, memory) (head instructions)

executeInstruction :: ([Int], Memory) -> Instruction -> ([Int], Memory)
executeInstruction (stdin, memory) (Inp (Variable v)) = (tail stdin, Map.insert v (head stdin) memory)
executeInstruction (stdin, memory) (Inp _) = error "Unsupported operand"
executeInstruction (stdin, memory) (Add lhs rhs) = (stdin, executeBinaryOperation memory (+) lhs rhs)
executeInstruction (stdin, memory) (Mul lhs rhs) = (stdin, executeBinaryOperation memory (*) lhs rhs)
executeInstruction (stdin, memory) (Div lhs rhs) = (stdin, executeBinaryOperation memory div lhs rhs)
executeInstruction (stdin, memory) (Mod lhs rhs) = (stdin, executeBinaryOperation memory mod lhs rhs)
executeInstruction (stdin, memory) (Eql lhs rhs) = (stdin, executeBinaryOperation memory eql lhs rhs)
  where
    eql lhs rhs = if lhs == rhs then 1 else 0

executeBinaryOperation :: Memory -> (Int -> Int -> Int) -> Operand -> Operand -> Memory
executeBinaryOperation memory fn lhs rhs = Map.insert lhsBinding (fn lhsValue rhsValue) memory
  where
    lhsBinding = retrieveBinding lhs
    lhsValue = retrieveValue memory lhs
    rhsValue = retrieveValue memory rhs
    retrieveValue memory (Literal value) = value
    retrieveValue memory (Variable binding) = fromMaybe (error "Memory not set") $ Map.lookup binding memory
    retrieveBinding (Variable binding) = binding
    retrieveBinding _ = error "Unsupported operand type"
