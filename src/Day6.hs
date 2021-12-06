{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day6 (day06a, day06b) where

import AoC.Parsing (int, run)
import Control.Applicative.Combinators (sepBy1, many)
import Text.Megaparsec.Char (char, eol)

occurences :: [Int] -> [Int]
occurences [] = replicate 9 0
occurences (e : es) = l <> (succ m : r)
  where
    (l, m : r) = splitAt e $ occurences es

step :: Num a => [a] -> [a]
step [z, a, b, c, d, e, f, g, h] = [a, b, c, d, e, f, g + z, h, z]

solve :: Int -> [Int] -> Int
solve days input =
  let go 0 i = i
      go days' i = go (pred days') (step i)
   in sum $ go days $ occurences input

day06a :: String -> String
day06a = show . solve 80 . run (sepBy1 int (char ',') <* many eol)

day06b :: String -> String
day06b = show . solve 256 . run (sepBy1 int (char ',') <* many eol )
