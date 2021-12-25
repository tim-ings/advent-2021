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
solve numbers = show $ maximum magnitudes
  where
    forwardPairs = zip numbers $ tail numbers
    backwardPairs = map swap forwardPairs
    pairs = forwardPairs ++ backwardPairs
    magnitudes = map (snailMagnitude . uncurry snailAdd) pairs

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

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
    reducedNumbers = iterateToStable (fst . snailSplit False . iterateSnailExplode) num

iterateSnailExplode :: SnailNumber -> SnailNumber
iterateSnailExplode num = if null explodedNumbers then num else last explodedNumbers
  where
    runSnailExplode num = result where (result, _, _, _) = snailExplode False 0 num
    explodedNumbers = iterateToStable runSnailExplode num

iterateToStable :: Eq a => (a -> a) -> a -> [a]
iterateToStable fn initial = map snd $ takeWhile (uncurry (/=)) $ iterate (\(_, next) -> (next, fn next)) (initial, fn initial)

snailExplode :: Bool -> Int -> SnailNumber -> (SnailNumber, Maybe Int, Maybe Int, Bool)
snailExplode True _ number = (number, Nothing, Nothing, True)
snailExplode done _ (SnailLiteral n) = (SnailLiteral n, Nothing, Nothing, False)
snailExplode done depth (SnailPair lhs rhs)
  | depth >= 4 = (SnailLiteral 0, extractLiteral lhs', extractLiteral rhs', True)
  | otherwise = (SnailPair lhs'' rhs'', lhsLeftCarry, rhsRightCarry, done')
  where
    (lhs', lhsLeftCarry, lhsRightCarry, lhsDone) = snailExplode done (succ depth) lhs
    (rhs', rhsLeftCarry, rhsRightCarry, rhsDone) = snailExplode lhsDone (succ depth) rhs
    lhs'' = applyLeftCarry lhs' rhsLeftCarry
    rhs'' = applyRightCarry rhs' lhsRightCarry
    done' = done || lhsDone || rhsDone

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

snailSplit :: Bool -> SnailNumber -> (SnailNumber, Bool)
snailSplit True number = (number, True)
snailSplit done (SnailPair lhs rhs) = (SnailPair lhs' rhs', done')
  where
    (lhs', lhsDone) = snailSplit done lhs
    (rhs', rhsDone) = snailSplit lhsDone rhs
    done' = done || lhsDone || rhsDone
snailSplit done (SnailLiteral n)
  | n >= 10 = (SnailPair (SnailLiteral lhs) (SnailLiteral rhs), True)
  | otherwise = (SnailLiteral n, done)
  where
    lhs = floor (fromIntegral n / 2.0)
    rhs = ceiling (fromIntegral n / 2.0)

snailMagnitude :: SnailNumber -> Int
snailMagnitude (SnailLiteral n) = n
snailMagnitude (SnailPair lhs rhs) = (3 * snailMagnitude lhs) + (2 * snailMagnitude rhs)
