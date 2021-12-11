{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Day10 (day10a, day10b) where

import Control.Monad.Combinators
import Data.Functor (($>))
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.Set as Set
import Text.Megaparsec (MonadParsec (observing, parseError), Parsec, customFailure, eof, registerFancyFailure, runParser)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error

data Chunk = Round [Chunk] | Square [Chunk] | Curly [Chunk] | Angle [Chunk] deriving (Show)

type NavParser = Parsec NavParserError String

data NavParserError = Incomplete Char | Corrupt Int deriving (Show, Eq, Ord)

chunks :: NavParser [Chunk]
chunks = do
  -- Catch and modify corrupt errors
  f <- observing chunks'
  case f of
    Left e@(TrivialError _ (Just (Tokens (u :| ""))) _) -> case u of
      ')' -> customFailure $ Corrupt 3
      ']' -> customFailure $ Corrupt 57
      '}' -> customFailure $ Corrupt 1197
      '>' -> customFailure $ Corrupt 25137
      _ -> parseError e
    Left a -> parseError a
    Right x -> return x
  where
    chunks' :: NavParser [Chunk]
    chunks' =
      many $
        choice
          [ Round <$> between' '(' ')',
            Square <$> between' '[' ']',
            Curly <$> between' '{' '}',
            Angle <$> between' '<' '>'
          ]
    -- Register error (missing closing) for later and continue
    between' open close = between (char open) (endOrFail close) chunks'
    endOrFail c = char c <|> eof *> registerFancyFailure (Set.singleton $ ErrorCustom $ Incomplete c) $> c

solveA :: String -> Int
solveA = either score (const 0) . runParser chunks ""
  where
    score (ParseErrorBundle errors _) = sum $ score' <$> errors
    score' TrivialError {} = 0
    score' (FancyError _ fancyErr) = sum $ f <$> Set.toList fancyErr
    f (ErrorCustom (Corrupt i)) = i
    f _ = 0

scoreMissing :: String -> Int
scoreMissing = foldr go 0
  where
    go ')' s = s * 5 + 1
    go ']' s = s * 5 + 2
    go '}' s = s * 5 + 3
    go '>' s = s * 5 + 4
    go _ s = s

missingScore :: String -> Int
missingScore = scoreMissing . either getMissing (const []) . runParser chunks ""
  where
    getMissing (ParseErrorBundle errors _) = getMissing' =<< toList errors
    getMissing' (FancyError _ fancyErr) = getMissingChar =<< Set.toList fancyErr
    getMissing' _ = []
    getMissingChar (ErrorCustom (Incomplete c)) = [c]
    getMissingChar _ = []

day10a :: String -> String
day10a = show . sum . fmap solveA . lines

day10b :: String -> String
day10b = show . (\xs -> xs !! (length xs `div` 2)) . sort . filter (> 0) . fmap missingScore . lines
