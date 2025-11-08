module Main where

import Common (solvePart1, solvePart2)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure

sampleInput :: String
sampleInput =
  unlines
    [ "199",
      "200",
      "208",
      "210",
      "200",
      "207",
      "240",
      "269",
      "260",
      "263"
    ]

allTests :: Test
allTests =
  TestList
    [ TestLabel "part1_sample" $
        TestCase $
          assertEqual "Sample input should have 7 increases" 7 (solvePart1 sampleInput),
      TestLabel "part2_sample" $
        TestCase $
          assertEqual "Sample input should have 5 increases in sliding window sums" 5 (solvePart2 sampleInput)
    ]
