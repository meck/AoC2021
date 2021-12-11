{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Advent
import AoC (aocYear, solutionLookup)
import AoC.Api (prettyDay)
import Control.Arrow (first)
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (readFile)

getTests :: (Day, Part) -> IO [(String, String)]
getTests (d, p) = do
  solu <- readFile $ "test-data/" <> show (dayInt d) <> [partChar p] <> ".txt"
  pure $ case solu of
    "" -> []
    _ ->
      bimap (T.unpack . T.strip) (T.unpack . T.strip . T.drop 5)
        . T.breakOn ">>>> "
        <$> T.splitOn "<<<<\n" (T.strip solu)

makeTest :: (Eq a, Show a) => ((String, t -> a), [(t, a)]) -> TestTree
makeTest ((name, solu), tests) =
  testGroup name $
    ( \((tInp, tOut), i) -> testCase (show (i :: Int)) $ solu tInp @?= tOut
    )
      <$> zip tests [1 ..]

main :: IO ()
main = do
  testFiles <- sequenceA $ getTests . fst <$> solutionLookup
  defaultMain $
    testGroup ("AoC " <> show aocYear) $
      makeTest
        <$> zip
          (first (\(d, p) -> prettyDay (d, Just p)) <$> solutionLookup)
          testFiles
