module Day13 (day13a, day13b) where

import Advent.OCR (unsafeParseLetters)
import AoC.Parsing (Parser, cord, int, run, sepNL)
import AoC.Util (Cord)
import Control.Applicative ((<|>))
import Control.Arrow (Arrow (second))
import Data.Functor (($>))
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec.Char (char, space, string)

data Fold = Hor Int | Ver Int deriving (Show)

type Paper = Set Cord

foldPaper :: Paper -> Fold -> Paper
foldPaper p l = S.map (f l) p
  where
    f (Ver line) (x, y) | x > line = (2 * line - x, y)
    f (Hor line) (x, y) | y > line = (x, 2 * line - y)
    f _ c = c

foldparse :: Parser Fold
foldparse = do
  _ <- string "fold along "
  x <- (char 'x' $> Ver) <|> (char 'y' $> Hor)
  _ <- char '='
  x <$> int

input :: Parser (Paper, [Fold])
input = do
  c <- sepNL cord
  _ <- space
  f <- sepNL foldparse
  pure (S.fromList c, f)

day13a :: String -> String
day13a = show . S.size . uncurry foldPaper . second head . run input

day13b :: String -> String
day13b = unsafeParseLetters . uncurry (foldl' foldPaper) . run input
