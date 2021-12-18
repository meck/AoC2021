{-# LANGUAGE LambdaCase #-}

module Day16 (day16a, day16b) where

import AoC.Parsing (Parser, run)
import Control.Applicative.Combinators (count, (<|>))
import Data.Binary.Strict.BitGet (BitGet)
import qualified Data.Binary.Strict.BitGet as BG
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Word
import Foreign (fromBool)
import Text.Megaparsec (MonadParsec (lookAhead), eof, some)
import Text.Megaparsec.Char (hexDigitChar)

data TypeID = Sum | Prod | Mini | Maxi | LessT | GreatT | EqT deriving (Show)

data PkgData = Literal Int | Operator TypeID [Packet] deriving (Show)

data Packet = Packet Int PkgData deriving (Show)

sumVersions :: Packet -> Int
sumVersions (Packet v (Literal _)) = v
sumVersions (Packet v (Operator _ subpkgs)) = v + sum (sumVersions <$> subpkgs)

eval :: Packet -> Int
eval = \case
  (Packet _ (Literal i)) -> i
  (Packet _ (Operator Sum ps)) -> sum $ eval <$> ps
  (Packet _ (Operator Prod ps)) -> product $ eval <$> ps
  (Packet _ (Operator Mini ps)) -> minimum $ eval <$> ps
  (Packet _ (Operator Maxi ps)) -> maximum $ eval <$> ps
  (Packet _ (Operator LessT ps)) -> checkPair (<) ps
  (Packet _ (Operator GreatT ps)) -> checkPair (>) ps
  (Packet _ (Operator EqT ps)) -> checkPair (==) ps
  where
    checkPair f [a, b] = if eval a `f` eval b then 1 else 0
    checkPair _ _ = error "not two subpackets"

pPacket :: BitGet Packet
pPacket =
  Packet
    <$> (fromIntegral <$> BG.getAsWord8 3)
    <*> pData

pData :: BitGet PkgData
pData =
  BG.getAsWord8 3 >>= \case
    0 -> Operator Sum <$> operatorVal
    1 -> Operator Prod <$> operatorVal
    2 -> Operator Mini <$> operatorVal
    3 -> Operator Maxi <$> operatorVal
    4 -> Literal <$> litteralVal
    5 -> Operator GreatT <$> operatorVal
    6 -> Operator LessT <$> operatorVal
    7 -> Operator EqT <$> operatorVal
    _ -> fail "Invalid type ID"

litteralVal :: BitGet Int
litteralVal = foldl' (\i b -> shiftL i 1 .|. fromBool b) 0 <$> go
  where
    go = do
      isMore <- BG.getBit
      i <- count 4 BG.getBit
      if not isMore
        then pure i
        else (i <>) <$> go

operatorVal :: BitGet [Packet]
operatorVal = do
  lengthID <- BG.getBit
  if lengthID
    then do
      n <- fromIntegral <$> BG.getAsWord16 11
      count n pPacket
    else do
      len <- BG.getAsWord16 15
      foo <- BG.getLeftByteString (fromIntegral len)
      let bla = BG.runBitGet foo subPackets
      case bla of
        Left err -> fail err
        Right x -> pure x
  where
    subPackets :: BitGet [Packet]
    subPackets = do
      len <- BG.remaining
      foo <- BG.lookAhead $ BG.getLeftByteString len
      case BG.runBitGet foo pPacket of
        Left _ -> pure []
        Right _ -> (:) <$> pPacket <*> subPackets

parseBITS :: String -> Packet
parseBITS = either (error "bad parse") id . flip BG.runBitGet pPacket . BS.pack . run twoFourBitWords
  where
    twoFourBitWords = some $ do
      let fourB :: Parser Word8
          fourB = fromIntegral . digitToInt <$> hexDigitChar
      a <- fourB
      b <- fourB <|> lookAhead eof $> 0
      pure $ shiftL a 4 .|. b

day16a :: String -> String
day16a = show . sumVersions . parseBITS

day16b :: String -> String
day16b = show . eval . parseBITS
