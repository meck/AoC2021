{-# LANGUAGE OverloadedStrings #-}

module AoC.Parsing (Parser, run, lexeme, sepNL, int, hex, signedInt, symbol, parens) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Simplest type of parser
type Parser = Parsec Void String

-- Run Parser until eof
run :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Parsec e s p -> s -> p
run p s = case runParser (p <* eof) "" s of
  (Left err) -> error $ errorBundlePretty err
  (Right res) -> res

-- Space Between Tokens
-- Keep newlines
lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

-- newline Between Tokens
sepNL :: Parser a -> Parser [a]
sepNL = flip sepEndBy newline

-- Token parser
symbol :: Tokens [Char] -> Parser (Tokens [Char])
symbol = L.symbol hspace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Integer
int :: Parser Int
int = lexeme L.decimal

-- Hex
hex :: Parser Int
hex = lexeme L.hexadecimal

-- Signed Integer
-- No space between sign
signedInt :: Parser Int
signedInt = L.signed (pure ()) int
