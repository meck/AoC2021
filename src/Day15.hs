{-# LANGUAGE TupleSections #-}

module Day15 (day15a, day15b) where

import AoC.Parsing (intGrid, run)
import AoC.Util (Cord, cardinalNeighbours)
import Control.Arrow (first, second)
import Control.Monad.Search (MonadSearch (abandon), SearchT, cost', runSearchBestT)
import Control.Monad.State.Strict (State, evalState, gets, lift, modify)
import Data.Foldable (asum)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Sum (Sum, getSum))
import Data.Set (Set)
import qualified Data.Set as Set

type Cave = Map Cord Int

findCost :: Cave -> Maybe Int
findCost cave = getSum . fst <$> evalState (runSearchBestT $ go [(0, 0)]) Set.empty
  where
    target = maximum $ M.keys cave
    neighbours c = catMaybes $ (\x -> (x,) <$> (x `M.lookup` cave)) <$> cardinalNeighbours c
    go :: [Cord] -> SearchT (Sum Int) (State (Set Cord)) [Cord]
    go [] = abandon
    go allSteps@(currentStep : _)
      | currentStep == target = pure allSteps
      | otherwise = do
        alreadySeen <- lift $ gets (currentStep `Set.member`)
        if alreadySeen
          then abandon
          else do
            lift $ modify (Set.insert currentStep)
            asum $ (\(l, d) -> cost' (Sum d) >> go (l : allSteps)) <$> neighbours currentStep

expandGrid :: Cave -> Cave
expandGrid grid = grid'
  where
    (xMax, yMax) = maximum $ M.keys grid
    inc = fmap (\w -> if w == 9 then 1 else w + 1)
    shiftRight = M.mapKeys (second (+ succ xMax))
    shiftDown = M.mapKeys (first (+ succ yMax))
    expandedRow = M.unions $ take 5 $ iterate (inc . shiftRight) grid
    grid' = M.unions $ take 5 $ iterate (inc . shiftDown) expandedRow

day15a :: String -> String
day15a = show . fromJust . findCost . run intGrid

day15b :: String -> String
day15b = show . fromJust . findCost . expandGrid . run intGrid
