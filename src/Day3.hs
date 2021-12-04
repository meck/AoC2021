module Day3 (day03a, day03b) where

import AoC.Parsing (Parser, run, sepNL)
import Control.Applicative ((<|>))
import Control.Applicative.Combinators (some)
import Data.Bits (Bits (shiftL), (.|.))
import Data.List (foldl', transpose)
import Foreign (fromBool)
import Text.Megaparsec.Char (char)

mostCommonB :: [Bool] -> Bool
mostCommonB xs = length (filter id xs) >= (length xs /^ 2)
  where
    -- Integer ceiling division
    (/^) x y = (x + y - 1) `div` y

leastCommonB :: [Bool] -> Bool
leastCommonB = not . mostCommonB

bsToInt :: [Bool] -> Int
bsToInt = foldl' f 0
  where
    f i b = fromBool b .|. (i `shiftL` 1)

findWith :: ([Bool] -> Bool) -> [[Bool]] -> [Bool]
findWith crit = go 0
  where
    go _ [x] = x
    go n xs = go (succ n) bitC
      where
        commonBits = transpose xs
        bitC = filter (\bs -> bs !! n == crit (commonBits !! n)) xs

solveA :: [[Bool]] -> Int
solveA xs = gam * eps
  where
    commonBits = transpose xs
    eps = bsToInt $ mostCommonB <$> commonBits
    gam = bsToInt $ leastCommonB <$> commonBits

solveB :: [[Bool]] -> Int
solveB xs = oxi * co2
  where
    oxi = bsToInt $ findWith mostCommonB xs
    co2 = bsToInt $ findWith leastCommonB xs

binAsBs :: Parser [Bool]
binAsBs = some $ ('1' ==) <$> (char '0' <|> char '1')

day03a :: String -> String
day03a = show . solveA . run (sepNL binAsBs)

day03b :: String -> String
day03b = show . solveB . run (sepNL binAsBs)
