module Day5 (day05a, day05b) where

import AoC.Parsing (Parser, int, run, sepNL)
import AoC.Util (group)
import Data.List (sort)
import Text.Megaparsec.Char (char, string)

type Cord = (Int, Int)

lineCords :: (Cord, Cord) -> [Cord]
lineCords ((x1, y1), (x2, y2)) =
  [(x1 + i * dirX, y1 + i * dirY) | i <- [0 .. len]]
  where
    dir a b
      | a < b = 1
      | a > b = -1
      | otherwise = 0
    len = maximum [abs (x1 - x2), abs (y1 - y2)]
    dirX = dir x1 x2
    dirY = dir y1 y2

isStraight :: (Cord, Cord) -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

line :: Parser (Cord, Cord)
line = do
  x1 <- int <* char ','
  y1 <- int <* string "-> "
  x2 <- int <* char ','
  y2 <- int
  pure ((x1, y1), (x2, y2))

day05a :: String -> String
day05a =
  show
    . length
    . filter ((> 1) . length)
    . group
    . sort
    . concatMap lineCords
    . filter isStraight
    . run (sepNL line)

day05b :: String -> String
day05b =
  show
    . length
    . filter ((> 1) . length)
    . group
    . sort
    . concatMap lineCords
    . run (sepNL line)
