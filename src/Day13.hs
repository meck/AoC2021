module Day13 (day13a, day13b) where

import AoC.Parsing (Parser, cord, int, run, sepNL)
import AoC.Util (Cord, drawCords, strip)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (scanl')
import qualified Data.Map as M
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

printPaper :: Paper -> String
printPaper s = drawCords '.' id $ M.fromList $ zip (S.toList s) (repeat '#')

day13a :: String -> String
day13a = show . S.size . (!! 1) . uncurry (scanl' foldPaper) . run input

day13b :: String -> String
day13b = strip . printPaper . last . uncurry (scanl' foldPaper) . run input
