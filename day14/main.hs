import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)
import Data.Function (on)

type Polymer = String
type PolymerPair = (Char, Char)
type PairInsertionRules = Map PolymerPair Char

type Case = (Polymer, PairInsertionRules)
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve (polymer, rules) = mostFrequentCount - lestFrequentCount
  where
    maturePolymer = growPolymer rules 10 polymer
    frequencyMap = buildFrequencyMap Map.empty maturePolymer
    frequencies = sortBy (compare `on` snd) $ Map.toList frequencyMap
    (_, lestFrequentCount) = head frequencies
    (_, mostFrequentCount) = last frequencies

readCase :: String -> Case
readCase raw = (polymer, insertionRules)
  where
    rawLines = lines raw
    polymer = readPolymer rawLines
    rawInsertionRules = tail $ tail rawLines
    insertionRules = Map.fromList $ map readInsertionRule rawInsertionRules

readPolymer :: [String] -> Polymer
readPolymer = head

readInsertionRule :: String -> ((Char, Char), Char)
readInsertionRule (a : b : ' ':'-':'>':' ' : c : _) = ((a, b), c)
readInsertionRule _ = error "Failed to parse insertion rule"

growPolymer :: PairInsertionRules -> Int -> Polymer -> Polymer
growPolymer rules 0 polymer = polymer
growPolymer rules n polymer = growPolymer rules (pred n) (expandPairs (head polymer) rules (zip polymer $ tail polymer))

expandPairs :: Char -> PairInsertionRules -> [PolymerPair] -> Polymer
expandPairs firstChar rules = foldl' (\acc pair -> acc ++ expandPair rules pair) [firstChar]

expandPair :: PairInsertionRules -> PolymerPair -> Polymer
expandPair rules (a, b) = c : [b]
  where
    c = fromMaybe (error "Failed to find rule for pair") $ Map.lookup (a, b) rules

buildFrequencyMap :: Map Char Int -> Polymer -> Map Char Int
buildFrequencyMap frequencies [] = frequencies
buildFrequencyMap frequencies polymer = buildFrequencyMap (Map.insertWith (+) (head polymer) 1 frequencies) (tail polymer)
