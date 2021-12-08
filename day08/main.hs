import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Digit = Set Char
type Display = ([Digit], [Digit])
type Case = [Display]
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve = sum . map decodeOutput

readCase :: String -> Case
readCase = map readDisplay . lines

readDisplay :: String -> Display
readDisplay rawCase = (map Set.fromList inputSignals, map Set.fromList outputSignals)
  where
    (inputs, outputs) = break (== '|') rawCase
    inputSignals = words inputs
    outputSignals = words $ tail outputs

decodeOutput :: Display -> Int
decodeOutput (inputs, outputs) = parseDigits outputDigits
  where
    wiring = deduceDisplayWiring inputs
    outputDigits = map (\digit -> unwrap $ Map.lookup digit wiring) outputs

deduceDisplayWiring :: [Digit] -> Map Digit Int
deduceDisplayWiring inputs = signalsMap
  where
    groups = groupBySize Map.empty inputs
    [signal1] = unwrap $ Map.lookup 2 groups
    [signal4] = unwrap $ Map.lookup 4 groups
    [signal7] = unwrap $ Map.lookup 3 groups
    [signal8] = unwrap $ Map.lookup 7 groups

    signals069 = unwrap $ Map.lookup 6 groups
    ([signal9], signals06) = partition (signal4 `Set.isSubsetOf`) signals069
    ([signal0], [signal6]) = partition (signal1 `Set.isSubsetOf`) signals06

    signals235 = unwrap $ Map.lookup 5 groups
    (signals35, [signal2]) = partition (`Set.isSubsetOf` signal9) signals235
    ([signal5], [signal3]) = partition (`Set.isSubsetOf` signal6) signals35

    signalValues = [signal0, signal1, signal2, signal3, signal4, signal5, signal6, signal7, signal8, signal9]
    signalsMap = Map.fromList $ zip signalValues [0 .. 9]

groupBySize :: Map Int [Digit] -> [Digit] -> Map Int [Digit]
groupBySize groups [] = groups
groupBySize groups sets = groupBySize updatedGroups (tail sets)
  where
    nextSet = head sets
    updatedGroups = Map.insertWith (++) (Set.size nextSet) [nextSet] groups

parseDigits :: [Int] -> Int
parseDigits digits = sum components
  where
    powers = iterate (* 10) 1
    components = zipWith (*) powers $ reverse digits

unwrap :: Maybe a -> a
unwrap a =
  case a of
    Just value -> value
    Nothing -> error "Value was nothing"
