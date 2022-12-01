module AoC21E01 (sonarSweep) where

import Helpers (printHeader, readData)

sonarSweep :: IO ()
sonarSweep = do
  printHeader "2021 Day 1: Sonar Sweep"
  result <- processData "data-s21e01-sample.txt"
  putStrLn ("Sample: " ++ show result)

processData :: FilePath -> IO Int
processData file = do
  content <- readData file
  return (process content)
  where
    process = countIncreasing . parseInput

parseInput :: String -> [Int]
parseInput = map read . lines

countIncreasing :: [Int] -> Int
countIncreasing = length . filter isIncreasing . pairs
  where
    pairs = zip <*> tail
    isIncreasing (a, b) = a < b
