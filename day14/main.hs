import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Function (on)

type Polymer = String
type PolymerPair = (Char, Char)
type PairInsertionRules = Map PolymerPair Char
type PolymerPairCounts = Map PolymerPair Int

type Case = (Polymer, PolymerPairCounts, PairInsertionRules)
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve (polymer, pairCounts, rules) = mostFrequentCount - leastFrequentCount
  where
    maturePairCounts = growPolymer rules 40 pairCounts
    frequencyMap = buildFrequencyMap polymer maturePairCounts
    frequencies = sortBy (compare `on` snd) $ Map.toList frequencyMap
    (_, leastFrequentCount) = head frequencies
    (_, mostFrequentCount) = last frequencies

readCase :: String -> Case
readCase raw = (polymer, pairCounts, insertionRules)
  where
    rawLines = lines raw
    polymer = head rawLines
    pairCounts = readPairCounts polymer
    rawInsertionRules = tail $ tail rawLines
    insertionRules = Map.fromList $ map readInsertionRule rawInsertionRules

readPairCounts :: String -> PolymerPairCounts
readPairCounts polymer = Map.fromListWith (+) $ zip pairs (repeat 1)
  where
    pairs = zip polymer (tail polymer)

readInsertionRule :: String -> ((Char, Char), Char)
readInsertionRule (a : b : ' ':'-':'>':' ' : c : _) = ((a, b), c)
readInsertionRule _ = error "Failed to parse insertion rule"

growPolymer :: PairInsertionRules -> Int -> PolymerPairCounts -> PolymerPairCounts
growPolymer _ 0 pairCounts = pairCounts
growPolymer rules n  pairCounts = growPolymer rules (pred n) (expandPairs rules pairCounts)

expandPairs :: PairInsertionRules -> PolymerPairCounts -> PolymerPairCounts
expandPairs rules pairCounts = Map.unionWith (+) leftExpandedPairs rightExpandedPairs
  where
    leftExpandedPairs = Map.mapKeysWith (+) (expandLeft rules) pairCounts
    rightExpandedPairs = Map.mapKeysWith (+) (expandRight rules) pairCounts

expandLeft :: PairInsertionRules -> PolymerPair -> PolymerPair
expandLeft rules (left, right) = (unwrap $ Map.lookup (left, right) rules, right)

expandRight :: PairInsertionRules -> PolymerPair -> PolymerPair
expandRight rules (left, right) = (left, unwrap $ Map.lookup (left, right) rules)

unwrap :: Maybe a -> a
unwrap = fromMaybe (error "Failed to unwrap maybe")

buildFrequencyMap :: Polymer -> PolymerPairCounts -> Map Char Int
buildFrequencyMap polymer pairCounts = deduplicateCounts rawElementCounts
  where
    fstCounts = Map.mapKeysWith (+) fst pairCounts
    sndCounts = Map.mapKeysWith (+) snd pairCounts
    rawElementCounts = Map.unionWith (+) fstCounts sndCounts
    deduplicateCounts = Map.adjust succ (head polymer) . Map.adjust succ (last polymer) . Map.map (`div` 2)
