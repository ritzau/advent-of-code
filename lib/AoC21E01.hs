module AoC21E01(s21E01) where

import Paths_AdventOfCode(getDataFileName)

countIncreasing :: [Int] -> Int
countIncreasing = length . filter isIncreasing . pairs
  where 
    pairs = zip <*> tail
    isIncreasing (a,b) = a < b

parseInput :: String -> [Int]
parseInput = map read . lines

processData :: FilePath -> IO Int
processData fileName = do
  path <- getDataFileName fileName
  content <- readFile path
  return (process content)
  where
    process = countIncreasing . parseInput

s21E01 = do
  putStrLn "AoC 2021e01"
  result <- processData "data-s21e01-sample.txt"
  putStrLn ("Sample: " ++ show result)
