module Day9 (day09a, day09b) where

import AoC.Parsing (Parser, run, sepNL)
import AoC.Util (cardinalNeighbours, mkCordsGrid)
import Control.Applicative (some)
import Control.Arrow (first)
import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Ord (Down (Down))
import Text.Megaparsec.Char (digitChar)

type Cord = (Int, Int)

type Floor = Map (Int, Int) Int

findBasinAt :: Floor -> Cord -> (Int, Floor)
findBasinAt fl c = case fl M.!? c of
  Nothing -> (0, nextFloor)
  Just 9 -> (0, nextFloor)
  Just _ -> foldr go (1, nextFloor) (cardinalNeighbours c)
  where
    nextFloor = M.delete c fl
    go c' (acc, fl') = first (acc +) $ findBasinAt fl' c'

findBasins :: Floor -> [Int]
findBasins fl | M.null fl = []
findBasins fl = filter (> 0) $ basinSize : findBasins fl'
  where
    c = fst $ head $ M.toList fl
    (basinSize, fl') = findBasinAt fl c

solveB :: Floor -> Int
solveB = product . take 3 . sortOn Down . findBasins

solveA :: Floor -> Int
solveA floorMap = M.foldr' ((+) . succ) 0 $ M.filterWithKey go floorMap
  where
    go pos h = and $ fmap (> h) $ catMaybes $ (floorMap M.!?) <$> cardinalNeighbours pos

floorPar :: Parser Floor
floorPar = mkCordsGrid id <$> sepNL (some (digitToInt <$> digitChar))

day09a :: String -> String
day09a = show . solveA . run floorPar

day09b :: String -> String
day09b = show . solveB . run floorPar
