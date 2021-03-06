module Day7 (day07a, day07b) where

import AoC.Parsing (run, intList)
import Data.List (sort)

solveA :: Integral a => [a] -> a
solveA xs = sum $ abs . (-) median <$> xs
  where
    median = sort xs !! (succ (length xs) `div` 2)

solveB :: (Foldable t, Functor t, Integral a) => t a -> a
solveB xs = min higherAve lowerAve
  where
    average :: Double
    average = fromIntegral (sum xs) / fromIntegral (length xs)
    cost y = sum $ (\x -> x * (x + 1) `div` 2) . abs . (-) y <$> xs
    higherAve = cost $ ceiling average
    lowerAve = cost $ floor average

day07a :: String -> String
day07a = show . solveA . run intList

day07b :: String -> String
day07b = show . solveB . run intList
