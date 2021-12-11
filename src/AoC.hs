module AoC (aocYear, solutionLookup) where

import Advent
import Control.Arrow (Arrow (first), (>>>))
import Day1
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day2
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9

aocYear :: Integer
aocYear = 2021

solutionLookup :: [((Day, Part), String -> String)]
solutionLookup =
  (first >>> first) mkDay_
    <$> [ ((1, Part1), day01a),
          ((1, Part2), day01b),
          ((2, Part1), day02a),
          ((2, Part2), day02b),
          ((3, Part1), day03a),
          ((3, Part2), day03b),
          ((4, Part1), day04a),
          ((4, Part2), day04b),
          ((5, Part1), day05a),
          ((5, Part2), day05b),
          ((6, Part1), day06a),
          ((6, Part2), day06b),
          ((7, Part1), day07a),
          ((7, Part2), day07b),
          ((8, Part1), day08a),
          ((8, Part2), day08b),
          ((9, Part1), day09a),
          ((9, Part2), day09b),
          ((10, Part1), day10a),
          ((10, Part2), day10b),
          ((11, Part1), day11a),
          ((11, Part2), day11b),
          ((12, Part1), day12a),
          ((12, Part2), day12b),
          ((13, Part1), day13a),
          ((13, Part2), day13b),
          ((14, Part1), day14a),
          ((14, Part2), day14b),
          ((15, Part1), day15a),
          ((15, Part2), day15b),
          ((16, Part1), day16a),
          ((16, Part2), day16b),
          ((17, Part1), day17a),
          ((17, Part2), day17b),
          ((18, Part1), day18a),
          ((18, Part2), day18b),
          ((19, Part1), day19a),
          ((19, Part2), day19b),
          ((20, Part1), day20a),
          ((20, Part2), day20b),
          ((21, Part1), day21a),
          ((21, Part2), day21b),
          ((22, Part1), day22a),
          ((22, Part2), day22b),
          ((23, Part1), day23a),
          ((23, Part2), day23b),
          ((24, Part1), day24a),
          ((24, Part2), day24b),
          ((25, Part1), day25a),
          ((25, Part2), day25b)
        ]
