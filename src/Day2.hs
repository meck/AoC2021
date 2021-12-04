module Day2 (day02a, day02b) where

import AoC.Parsing (Parser, int, run, sepNL, symbol)
import Control.Applicative ((<|>))
import Data.List (foldl')

data Inst = Down Int | Up Int | Forward Int

inst :: Parser Inst
inst = for <|> down <|> up
  where
    for = Forward <$> (symbol "forward" >> int)
    down = Down <$> (symbol "down" >> int)
    up = Up <$> (symbol "up" >> int)

type State = (Int, Int)

stepA :: State -> Inst -> State
stepA (h, d) i = case i of
  (Down x) -> (h, d + x)
  (Up x) -> (h, d - x)
  (Forward x) -> (h + x, d)

type StateWithAim = (Int, Int, Int)

stepB :: StateWithAim -> Inst -> StateWithAim
stepB (a, h, d) i = case i of
  (Down x) -> (a + x, h, d)
  (Up x) -> (a - x, h, d)
  (Forward x) -> (a, h + x, d + (a * x))

day02a :: String -> String
day02a = show . uncurry (*) . foldl' stepA (0, 0) . run (sepNL inst)

day02b :: String -> String
day02b = show . (\(_, h, d) -> h * d) . foldl' stepB (0, 0, 0) . run (sepNL inst)
