module Main (main) where

import Advent
import AoC
import AoC.Api (getDay, getInput, prettyDay)
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.Exit (die)

solvePart :: (Day, Part) -> IO ()
solvePart dp@(d, p) = do
  eInp <- getInput d
  case eInp of
    (Left e) -> die e
    (Right i) -> do
      let sol = fromJust $ dp `lookup` solutionLookup
      putStrLn $ ("Solution to day " <> prettyDay (d, Just p) <> " is:\n") <> sol i

main :: IO ()
main = do
  args <- getArgs
  es <-
    if null args
      then pure $ fst <$> solutionLookup
      else do
        let dayS = head args
            mDay = getDay dayS
        case mDay of
          Nothing -> die $ "\"" <> dayS <> "\" is not a valid exercise."
          Just (d, Nothing) -> pure [(d, Part1), (d, Part2)]
          Just (d, Just p) -> pure [(d, p)]
  traverse_ solvePart es
