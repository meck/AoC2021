{-# LANGUAGE TupleSections #-}

module Day11 (day11a, day11b) where

import AoC.Parsing
import AoC.Util (Cord, neighbours)
import Data.List (findIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

type Octos = Map Cord Int

step' :: (Octos, Octos) -> (Octos, Octos)
step' i@(haveFlashed, haveNotFlashed) =
  if M.null flashing
    then i
    else step' (flashing <> haveFlashed, updatedNeigh)
  where
    flashing = M.filter (> 9) haveNotFlashed
    clearFlashed = haveNotFlashed `M.difference` flashing
    neig = M.keys flashing >>= neighbours
    updatedNeigh = foldr (M.adjust succ) clearFlashed neig

step :: (Int, Octos) -> (Int, Octos)
step (nFlash, i) = (M.size flashed + nFlash, reset)
  where
    (flashed, notFlashed) = step' (M.empty, M.map succ i)
    reset = notFlashed <> M.map (const 0) flashed

allLit :: Octos -> Bool
allLit = (==0) . M.foldr' (+) 0

day11a :: String -> String
day11a = show . fst . (!! 100) . iterate step . (0,) . run intGrid

day11b :: String -> String
day11b = show . fromJust . findIndex (allLit . snd) . iterate step . (0,) . run intGrid
