module Main where

import Common (solvePart1, solvePart2)
import System.Exit (exitFailure, exitSuccess)

sampleInput = "987654321111111\n811111111111119\n234234234234278\n818181911112111\n"

sampleInput2 = "3354533332333324343135355544433311324132422234444325333334323542434333323236543423444343227424343334"

main :: IO ()
main = do
  let part1Result = solvePart1 sampleInput
  let part2Result = solvePart2 sampleInput

  putStrLn "Running tests..."

  -- Test Part 1
  if part1Result == 357
    then putStrLn "[PASS] Part 1 test passed"
    else do
      putStrLn $ "[FAIL] Part 1 test failed: expected 357, got " ++ show part1Result
      exitFailure

  -- Test Part 2
  if part2Result == 3121910778619
    then putStrLn "[PASS] Part 2 test passed"
    else do
      putStrLn $ "[FAIL] Part 2 test failed: expected 3121910778619, got " ++ show part2Result
      exitFailure

  putStrLn "All tests passed!"
  exitSuccess
