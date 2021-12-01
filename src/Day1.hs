module Day1 (day01a, day01b) where

import AoC.Parsing (int, run, sepNL)

solveA :: Ord a => [a] -> Int
solveA xs = length $ filter id $ zipWith (<) xs (tail xs)

solveB :: (Ord a, Num a) => [a] -> Int
solveB = solveA . foldr (zipWith (+)) (repeat 0) . take 3 . iterate tail

day01a :: String -> String
day01a = show . solveA . run (sepNL int)

day01b :: String -> String
day01b = show . solveB . run (sepNL int)
