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

allTests :: Test
allTests =
  TestList
    [ TestLabel "part1_sample_1" $
        TestCase $
          assertEqual "Sample test" 0 (solvePart1 "sample input"),
      TestLabel "part2_sample_1" $
        TestCase $
          assertEqual "Sample test" 0 (solvePart2 "sample input")
    ]
