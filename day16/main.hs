import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import qualified Text.Parsec as Parsec
import Numeric (readHex)
import Text.Printf (printf)
import Data.Char (digitToInt)

type Case = Packet
type Soln = Int

type Version = Int
type TypeId = Int
data Packet
  = LiteralPacket Version Int
  | OperatorPacket Version TypeId [Packet]
  deriving (Show, Eq, Ord)

main :: IO ()
main = interact handle

handle :: String -> String
handle = show . solve . readCase

solve :: Case -> Soln
solve = versionSum

versionSum :: Packet -> Int
versionSum (LiteralPacket version _) = version
versionSum (OperatorPacket version _ subPackets) = version + sum (map versionSum subPackets)

readCase :: String -> Case
readCase rawInput =
  case Parsec.parse parsePacket "" (concatMap hexToBin rawInput) of
    Left err -> error $ show err
    Right packet -> packet

hexToBin :: Char -> String
hexToBin char =
  case readHex [char] of
    (digit, _) : _ -> printf "%04b" (digit :: Int)
    _ -> error "Failed to parse string as hex"

parsePacket :: Parser Packet
parsePacket = Parsec.choice [Parsec.try parseOperatorPacket, Parsec.try parseLiteralPacket] 

parseLiteralPacket :: Parser Packet
parseLiteralPacket = do
  version <- parseBinString 3
  Parsec.string "100"
  standardGroups <- Parsec.many parseStandardGroup
  terminalGroup <- parseTerminalGroup
  return $ LiteralPacket version (binToInt (concat standardGroups ++ terminalGroup))
  where
    parseStandardGroup = do
      Parsec.char '1'
      Parsec.count 4 parseBinDigit
    parseTerminalGroup = do
      Parsec.char '0'
      Parsec.count 4 parseBinDigit

unwrap :: Show a => Either a b -> b
unwrap (Left err) = error $ show err
unwrap (Right v) = v

parseOperatorPacket :: Parser Packet
parseOperatorPacket = Parsec.choice [Parsec.try parseLengthType0, Parsec.try parseLengthType1]
  where
    parseLengthType0 = do
      version <- parseBinString 3
      typeId <- parseBinString 3
      Parsec.char '0'
      subPacketsTotalLength <- parseBinString 15
      subPacketBits <- Parsec.count subPacketsTotalLength parseBinDigit
      let subPackets = unwrap $ Parsec.parse (Parsec.manyTill parsePacket Parsec.eof) "" subPacketBits
      return (OperatorPacket version typeId subPackets)
    parseLengthType1 = do
      version <- parseBinString 3
      typeId <- parseBinString 3
      Parsec.char '1'
      subPacketCount <- parseBinString 11
      subPackets <- Parsec.count subPacketCount parsePacket
      return $ OperatorPacket version typeId subPackets

parseBinDigit :: Parser Char
parseBinDigit = Parsec.oneOf "01"

parseBinString :: Int -> Parser Int
parseBinString n = binToInt <$> Parsec.count n parseBinDigit

binToInt :: String -> Int
binToInt = sum . zipWith (*) (iterate (2*) 1) . map digitToInt . reverse
