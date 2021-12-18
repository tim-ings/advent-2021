import Data.List.Split (splitOn)
import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import qualified Text.Parsec as ParsecString
import Debug.Trace (traceShow)

data SnailNumber
  = SnailPair SnailNumber SnailNumber
  | SnailLiteral Int
  deriving (Eq, Ord)

instance Show SnailNumber where
  show (SnailLiteral n) = show n
  show (SnailPair lhs rhs) = "[" ++ show lhs ++ "," ++ show rhs ++ "]"

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

snailReduce :: SnailNumber -> SnailNumber
snailReduce num = if null reducedNumbers then num else last reducedNumbers
  where
    reducedNumbers = iterateToStable (snailSplit False . iterateSnailExplode) num

iterateSnailExplode :: SnailNumber -> SnailNumber
iterateSnailExplode num = if null explodedNumbers then num else last explodedNumbers
  where
    runSnailExplode num = result where (result, _, _) = snailExplode False 0 num
    explodedNumbers = iterateToStable runSnailExplode num

iterateToStable :: Eq a => (a -> a) -> a -> [a]
iterateToStable fn initial = map snd $ takeWhile (uncurry (/=)) $ iterate (\(_, next) -> (next, fn next)) (initial, fn initial)

snailExplode :: Bool -> Int -> SnailNumber -> (SnailNumber, Maybe Int, Maybe Int)
snailExplode True _ num = (num, Nothing, Nothing)
snailExplode done _ (SnailLiteral n) = (SnailLiteral n, Nothing, Nothing)
snailExplode done depth (SnailPair lhs rhs)
  | depth >= 4 = (SnailLiteral 0, extractLiteral lhs', extractLiteral rhs')
  | otherwise = (SnailPair lhs'' rhs'', lhsLeftCarry, rhsRightCarry)
  where
    (lhs', lhsLeftCarry, lhsRightCarry) = snailExplode done (succ depth) lhs
    (rhs', rhsLeftCarry, rhsRightCarry) = snailExplode leftExploded (succ depth) rhs
    leftExploded = lhs /= lhs'
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

snailSplit :: Bool -> SnailNumber -> SnailNumber
snailSplit done (SnailPair lhs rhs) = SnailPair lhs' rhs'
  where
    lhs' = snailSplit False lhs
    rhs' = snailSplit leftSplit rhs
    leftSplit = lhs /= lhs'
snailSplit done (SnailLiteral n)
  | done = SnailLiteral n
  | n >= 10 = SnailPair (SnailLiteral lhs) (SnailLiteral rhs)
  | otherwise = SnailLiteral n
  where
    lhs = floor (fromIntegral n / 2.0)
    rhs = ceiling (fromIntegral n / 2.0)

snailMagnitude :: SnailNumber -> Int
snailMagnitude (SnailLiteral n) = n
snailMagnitude (SnailPair lhs rhs) = (3 * snailMagnitude lhs) + (2 * snailMagnitude rhs)
