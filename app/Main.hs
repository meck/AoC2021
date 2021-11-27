module Main (main) where

import AoC
import AoC.Input (getInput)
import System.Environment (getArgs)
import System.Exit (die)

solvePart :: (String, String -> String) -> IO ()
solvePart (name, solutionFn) = do
  mInput <- getInput name
  case mInput of
    Nothing -> putStrLn $ "No input for puzzle " <> name
    (Just input) -> putStrLn $ ("Solution to " <> name <> " is:\n") <> solutionFn input

main :: IO ()
main = do
  args <- getArgs
  if null args
    then sequence_ $ solvePart <$> solutionLookup
    else
      let n = head args
       in case lookup n solutionLookup of
            Nothing -> die $ n ++ " is not a valid exercise"
            Just p -> solvePart (n, p)
