{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AoC
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (readFile)

getTests :: String -> IO [(String, String)]
getTests name = do
  solu <- readFile $ "test-data/" ++ name ++ ".txt"
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
  defaultMain $ testGroup ("AoC " <> show aocYear) $ makeTest <$> zip solutionLookup testFiles
