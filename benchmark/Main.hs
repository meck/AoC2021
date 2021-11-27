{-# LANGUAGE LambdaCase #-}

module Main (main) where

import AoC
import AoC.Input (getInput)
import Data.Maybe (fromMaybe)
import Gauge.Main

makeTest :: (String, String -> b) -> Benchmark
makeTest (name, f) = env (fromMaybe "" <$> getInput name) $ \ inp -> bench name $ whnf f inp

main :: IO ()
main = defaultMain $ makeTest <$> solutionLookup
