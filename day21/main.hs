import Data.Map (Map)
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import qualified Text.Parsec as Parsec
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Debug.Trace (traceShow)

data GameState = GameState
  { player1Position :: Int
  , player2Position :: Int
  , player1Score :: Int
  , player2Score :: Int
  , lastDieRoll :: Int
  , rollCount :: Int
  } deriving (Show)

type Case = GameState
type Soln = Int

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve state = traceShow finalState $ losingScore * rollCount finalState
  where
    finalState = step $ last $ takeWhile (not . isGameOver) $ iterate step state
    losingScore = minimum [player1Score finalState, player2Score finalState]

readCase :: String -> Case
readCase raw = case Parsec.parse parseCase "" raw of
  Left err -> error $ show err
  Right value -> value

parseCase :: Parser Case
parseCase = do
  Parsec.string "Player 1 starting position: "
  player1Starting <- digitToInt <$> Parsec.digit
  Parsec.newline
  Parsec.string "Player 2 starting position: "
  player2Starting <- digitToInt <$> Parsec.digit
  return GameState { player1Position = player1Starting
                   , player2Position = player2Starting
                   , player1Score = 0
                   , player2Score = 0
                   , lastDieRoll = 0
                   , rollCount = 0
                   }

isGameOver :: GameState -> Bool
isGameOver state = (player1Score state >= 1000) || (player2Score state >= 1000)

step :: GameState -> GameState
step state = traceShow state $ state'
  where
    lastRoll = lastDieRoll state
    (player1Rolls, lastRoll') = roll3 lastRoll
    (player2Rolls, lastRoll'') = roll3 lastRoll'
    player1Position' = wrap 10 (player1Position state + sum player1Rolls)
    player2Position' = wrap 10 (player2Position state + sum player2Rolls)
    player1Score' = player1Score state + player1Position'
    player2Score' = player2Score state + player2Position'
    player1Wins = player1Score' >= 1000
    state' = GameState { player1Position = player1Position'
                       , player2Position = if player1Wins then player2Position state else player2Position'
                       , player1Score = player1Score'
                       , player2Score = if player1Wins then player2Score state else player2Score'
                       , lastDieRoll = if player1Wins then lastRoll' else lastRoll''
                       , rollCount = if player1Wins then rollCount state + 3 else rollCount state + 6
                       }

wrap :: Int -> Int -> Int
wrap limit n = if n' == 0 then limit else n'
  where
    n' = n `mod` limit

roll :: Int -> Int
roll lastRoll = if nextRoll == 0 then 100 else nextRoll
  where
    nextRoll = succ lastRoll `mod` 100

roll3 :: Int -> ([Int], Int)
roll3 n = (rolls, last rolls)
  where
    rolls = tail $ take 4 $ iterate roll n
