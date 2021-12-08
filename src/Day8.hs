{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day8 (day08a, day08b) where

import AoC.Parsing (Parser, run, sepNL)
import Control.Applicative.Combinators (sepBy1, some, someTill)
import Data.List (intersect, partition, sort, (\\))
import Data.Maybe (fromJust)
import Text.Megaparsec (chunk, oneOf)
import Text.Megaparsec.Char (hspace1)

type SignalLT = [(Char, Char)]

findWiring :: [String] -> SignalLT
findWiring xs = [(a, 'a'), (b, 'b'), (c, 'c'), (d, 'd'), (e, 'e'), (f, 'f'), (g, 'g')]
  where
    [one] = filter ((== 2) . length) xs
    [four] = filter ((== 4) . length) xs
    [seven] = filter ((== 3) . length) xs
    zeroSixNine = filter ((== 6) . length) xs
    twoThreeFive = filter ((== 5) . length) xs
    (zeroNine, [six]) = partition ((== 2) . length . intersect one) zeroSixNine
    ([nine], [zero]) = partition (elem d) zeroNine
    ([five], twoThree) = partition (null . (\\ six)) twoThreeFive
    ([two], [three]) = partition ((== 3) . length . intersect five) twoThree

    [a] = seven \\ one
    [b] = five \\ three
    [c] = nine \\ six
    [d] = foldl intersect four twoThreeFive
    [e] = zero \\ nine
    [f] = seven \\ two
    [g] = (nine \\ four) \\ seven

decode :: SignalLT -> String -> Int
decode lt = decodeSignal . sort . fmap (fromJust . (`lookup` lt))
  where
    decodeSignal s = case s of
      "abcefg" -> 0
      "cf" -> 1
      "acdeg" -> 2
      "acdfg" -> 3
      "bcdf" -> 4
      "abdfg" -> 5
      "abdefg" -> 6
      "acf" -> 7
      "abcdefg" -> 8
      "abcdfg" -> 9

type Input = ([String], [String])

solveA :: Input -> Int
solveA = length . filter ((`elem` [2, 4, 3, 7]) . length) . snd

solveB :: Input -> Int
solveB (input, output) = digitsToNumber $ map (decode wiring) output
  where
    wiring = findWiring input
    digitsToNumber = read . concatMap show

inputParser :: Parser Input
inputParser = do
  let val = some (oneOf ['a' .. 'g'])
  input <- someTill (val <* hspace1) (chunk "| ")
  output <- val `sepBy1` hspace1
  pure (input, output)

day08a :: String -> String
day08a = show . sum . fmap solveA . run (sepNL inputParser)

day08b :: String -> String
day08b = show . sum . fmap solveB . run (sepNL inputParser)
