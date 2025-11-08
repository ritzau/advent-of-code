module Main where

import Common (solvePart1, solvePart2)

main :: IO ()
main = do
  putStrLn ""
  putStrLn "--- 2021 Day 1: Sonar Sweep ---"
  putStrLn ""

  -- Read sample file
  sampleInput <- readFile "sample.txt"

  let part1Result = solvePart1 sampleInput
  putStrLn $ "Part 1 (sample): " ++ show part1Result

  let part2Result = solvePart2 sampleInput
  putStrLn $ "Part 2 (sample): " ++ show part2Result
  putStrLn ""
