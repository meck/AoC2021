module Day12 (day12a, day12b) where

import AoC.Parsing (Parser, run, sepNL)
import Control.Applicative ((<|>))
import Control.Applicative.Combinators (some)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec.Char (char, lowerChar, string, upperChar)

data Node = Start | BigCave String | SmallCave String | End deriving (Eq, Ord, Show)

type CaveSystem = Map Node (Set Node)

type Edge = (Node, Node)

countPaths :: Bool -> CaveSystem -> Int
countPaths = go Start S.empty
  where
    noRevisit (BigCave _) = False
    noRevisit _ = True
    go :: Node -> Set Node -> Bool -> CaveSystem -> Int
    go current visited canRevisit cave =
      case (current, current `S.member` visited) of
        (End, _) -> 1
        (Start, True) -> 0
        (_, True) | not canRevisit -> 0
        (_, seen) -> S.foldr step 0 (cave M.! current)
          where
            newVisited
              | not seen && noRevisit current = current `S.insert` visited
              | otherwise = visited
            step next acc = go next newVisited (canRevisit && not seen) cave + acc

mkCave :: [Edge] -> CaveSystem
mkCave = foldr addEdge M.empty
  where
    addEdge (x, y) c = M.unionWith S.union c (M.fromList [(x, S.singleton y), (y, S.singleton x)])

edges :: Parser Edge
edges = do
  a <- node
  _ <- char '-'
  b <- node
  pure (a, b)
  where
    node =
      (string "start" $> Start)
        <|> (string "end" $> End)
        <|> (BigCave <$> some upperChar)
        <|> (SmallCave <$> some lowerChar)

day12a :: String -> String
day12a = show . countPaths False . mkCave . run (sepNL edges)

day12b :: String -> String
day12b = show . countPaths True . mkCave . run (sepNL edges)
