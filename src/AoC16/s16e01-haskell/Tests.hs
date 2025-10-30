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
        TestCase $ assertEqual "Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away" 5 (solvePart1 "R2, L3"),
      TestLabel "part1_sample_2" $
        TestCase $ assertEqual "R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away" 2 (solvePart1 "R2, R2, R2"),
      TestLabel "part1_sample_3" $
        TestCase $ assertEqual "R5, L5, R5, R3 leaves you 12 blocks away" 12 (solvePart1 "R5, L5, R5, R3"),
      TestLabel "part2_sample_1" $
        TestCase $ assertEqual "R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East" 4 (solvePart2 "R8, R4, R4, R8")
    ]
