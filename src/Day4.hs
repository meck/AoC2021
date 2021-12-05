{-# LANGUAGE TupleSections #-}

module Day4 (day04a, day04b) where

import AoC.Parsing (Parser, int, run)
import Control.Applicative.Combinators (optional, sepBy1, some)
import Data.List (foldl', transpose)
import Text.Megaparsec.Char (char, eol, hspace, space1)

data Marker = Unmarked | Marked deriving (Eq, Show)

newtype BingoBoard = BB [[(Marker, Int)]] deriving (Eq, Show)

parseBoard :: Parser BingoBoard
parseBoard = BB <$> some (row <* optional eol)
  where
    row = hspace *> sepBy1 num hspace
    num = (Unmarked,) <$> int

isWinner :: BingoBoard -> Bool
isWinner (BB bb) = or $ isCompleteLine <$> allLines
  where
    allLines = transpose bb <> bb
    isCompleteLine xs = and $ (== Marked) . fst <$> xs

markBoard :: Int -> BingoBoard -> BingoBoard
markBoard m (BB bb) = BB $ fmap go <$> bb
  where
    go (_, x) | x == m = (Marked, x)
    go b = b

boardScore :: Int -> BingoBoard -> Int
boardScore m (BB bb) = m * foldr go 0 (concat bb)
  where
    go (Marked, _) a = a
    go (Unmarked, v) a = a + v

-- Score for winning and state
type State = ([Int], [BingoBoard])

stepBords :: State -> Int -> State
stepBords (winScores, s) m = foldr go (winScores, []) s
  where
    go b (winScores', s') =
      let markedBoard = markBoard m b
       in if isWinner markedBoard
            then (boardScore m markedBoard : winScores', s')
            else (winScores', markedBoard : s')

winnerScores :: [BingoBoard] -> [Int] -> [Int]
winnerScores bbs ms = fst (foldl' stepBords ([], bbs) ms)

parseInput :: Parser ([BingoBoard], [Int])
parseInput = do
  m <- sepBy1 int (char ',') <* some space1
  bbs <- sepBy1 parseBoard eol
  pure (bbs, m)

day04a :: String -> String
day04a =
  show
    . last
    . uncurry winnerScores
    . run parseInput

day04b :: String -> String
day04b =
  show
    . head
    . uncurry winnerScores
    . run parseInput
