module Main (main) where

import Advent
import AoC
import AoC.Api (getInput)
import Criterion.Main
import Data.Either (fromRight)

makeTest :: ((Day, Part), String -> b) -> Benchmark
makeTest ((d, p), f) = do
  env (fromRight "" <$> getInput d) $ \inp -> bench (show (dayInt d) <> [partChar p]) $ whnf f inp

main :: IO ()
main = defaultMain $ makeTest <$> solutionLookup
