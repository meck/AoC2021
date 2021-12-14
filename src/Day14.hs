module Day14 (day14a, day14b) where

import AoC.Parsing (Parser, run, sepNL)
import Control.Applicative (some)
import Control.Monad (ap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.Megaparsec.Char (space, string, upperChar)

type Pair = (Char, Char)

type Rule = (Pair, Char)

type Freq = Map Pair Int

inputParse :: Parser ([Rule], String)
inputParse = do
  template <- some upperChar <* space
  rules <- sepNL rule
  pure (rules, template)
  where
    rule = do
      t1 <- upperChar
      t2 <- upperChar <* string " -> "
      b <- upperChar
      pure ((t1, t2), b)

addToKey :: (Ord k, Num a) => a -> k -> Map k a -> Map k a
addToKey n = M.alter (Just . (+ n) . fromMaybe 0)

step :: [Rule] -> Freq -> Freq
step rules = M.foldlWithKey' go M.empty
  where
    go acc pair@(a, b) n =
      let mNew = do
            newChar <- pair `lookup` rules
            pure $ M.fromList [((a, newChar), n), ((newChar, b), n)]
       in case mNew of
            (Just new) -> M.unionWith (+) new acc
            Nothing -> acc

countRes :: Map (Char, b) Int -> Map Char Int
countRes = M.foldrWithKey' (flip addToKey . fst) M.empty

solveN :: Int -> ([Rule], String) -> Int
solveN n (rules, template) = maximum frq - minimum frq
  where
    n0 = M.fromListWith (+) $ flip zip (repeat 1) $ ap zip tail template
    nFinal = (!! n) $ iterate (step rules) n0
    frq = fmap snd $ M.toList $ addToKey 1 (last template) $ countRes nFinal

day14a :: String -> String
day14a = show . solveN 10 . run inputParse

day14b :: String -> String
day14b = show . solveN 40 . run inputParse
