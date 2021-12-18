import Data.List.Split (splitOn)
import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import qualified Text.Parsec as ParsecString

data SnailNumber
  = SnailPair SnailNumber SnailNumber
  | SnailLiteral Int
  deriving (Show, Eq, Ord)

type Case = [SnailNumber]
type Soln = String

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve numbers = show $ snailMagnitude $ snailSum (head numbers, tail numbers)

readCase :: String -> Case
readCase = map readLine . lines

readLine :: String -> SnailNumber
readLine rawLine = 
    case Parsec.parse parseSnailPair "" rawLine of
    Left err -> error $ show err
    Right snailNumber -> snailNumber

parseSnailLiteral = do
  digits <- Parsec.many (Parsec.digit <|> Parsec.oneOf ".-")
  return (SnailLiteral (read digits))

parseSnailPair = do
  Parsec.char '['
  lhs <- parseSnailPair <|> parseSnailLiteral
  Parsec.char ','
  rhs <- parseSnailPair <|> parseSnailLiteral
  Parsec.char ']'
  return (SnailPair lhs rhs)

snailSum :: (SnailNumber, [SnailNumber]) -> SnailNumber
snailSum (total, []) = total
snailSum (total, numbers) = snailSum (snailAdd total (head numbers), tail numbers)

snailAdd :: SnailNumber -> SnailNumber -> SnailNumber
snailAdd lhs rhs = snailReduce $ SnailPair lhs rhs

-- this is the problem function
-- we need to keep exploding/splitting properly
-- we may also then need to stop an action and start again from root if we do anything once
snailReduce :: SnailNumber -> SnailNumber
snailReduce num = splitNum
  where
    (explodedNum, _, _) = snailExplode 0 num
    splitNum = snailSplit 0 explodedNum

snailExplode :: Int -> SnailNumber -> (SnailNumber, Maybe Int, Maybe Int)
snailExplode _ (SnailLiteral n) = (SnailLiteral n, Nothing, Nothing)
snailExplode depth (SnailPair lhs rhs)
  | depth > 4 = (SnailLiteral 0, extractLiteral lhs', extractLiteral rhs')
  | otherwise = (SnailPair lhs'' rhs'', lhsLeftCarry, rhsRightCarry)
  where
    (lhs', lhsLeftCarry, lhsRightCarry) = snailExplode (succ depth) lhs
    (rhs', rhsLeftCarry, rhsRightCarry) = snailExplode (succ depth) rhs
    lhs'' = applyLeftCarry lhs' rhsLeftCarry
    rhs'' = applyRightCarry rhs' lhsRightCarry

extractLiteral :: SnailNumber -> Maybe Int
extractLiteral (SnailLiteral n) = Just n
extractLiteral _ = Nothing

applyRightCarry :: SnailNumber -> Maybe Int -> SnailNumber
applyRightCarry (SnailLiteral n) Nothing = SnailLiteral n
applyRightCarry (SnailLiteral n) (Just carry) = SnailLiteral (n + carry)
applyRightCarry (SnailPair lhs rhs) Nothing = SnailPair lhs rhs
applyRightCarry (SnailPair lhs rhs) (Just carry) = SnailPair (applyRightCarry lhs (Just carry)) rhs

applyLeftCarry :: SnailNumber -> Maybe Int -> SnailNumber
applyLeftCarry (SnailLiteral n) Nothing = SnailLiteral n
applyLeftCarry (SnailLiteral n) (Just carry) = SnailLiteral (n + carry)
applyLeftCarry (SnailPair lhs rhs) Nothing = SnailPair lhs rhs
applyLeftCarry (SnailPair lhs rhs) (Just carry) = SnailPair lhs (applyLeftCarry rhs (Just carry))

snailSplit :: Int -> SnailNumber -> SnailNumber
snailSplit depth (SnailPair lhs rhs) = SnailPair (snailSplit (succ depth) lhs) (snailSplit (succ depth) rhs)
snailSplit depth (SnailLiteral n)
  | depth > 4 = SnailPair (SnailLiteral lhs) (SnailLiteral rhs)
  | otherwise = SnailLiteral n
  where
    lhs = floor (fromIntegral n / 2.0)
    rhs = ceiling (fromIntegral n / 2.0)

snailMagnitude :: SnailNumber -> Int
snailMagnitude (SnailLiteral n) = n
snailMagnitude (SnailPair lhs rhs) = (3 * snailMagnitude lhs) + (2 * snailMagnitude rhs)
