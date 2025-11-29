module Main where

import Common (solvePart1, solvePart2)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  let part1Result = solvePart1 "sample input"
  let part2Result = solvePart2 "sample input"

  putStrLn "Running tests..."

  -- Test Part 1
  if part1Result == 0
    then putStrLn "[PASS] Part 1 test passed"
    else do
      putStrLn $ "[FAIL] Part 1 test failed: expected 0, got " ++ show part1Result
      exitFailure

  -- Test Part 2
  if part2Result == 0
    then putStrLn "[PASS] Part 2 test passed"
    else do
      putStrLn $ "[FAIL] Part 2 test failed: expected 0, got " ++ show part2Result
      exitFailure

  putStrLn "All tests passed!"
  exitSuccess
