module AoC22E01 (calorieCounting) where

import Data.Sort (sort)
import Helpers (printHeader, readData)

calorieCounting :: IO ()
calorieCounting = do
  printHeader "2022 Day 1: Calorie Counting"

  result <- maxCaloriesOf "data-s22e01-sample.txt"
  putStrLn ("Sample:   " ++ show result)

  result <- maxCaloriesOf "data-s22e01.txt"
  putStrLn ("Part 1:   " ++ show result)

  result <- top3SumOf "data-s22e01-sample.txt"
  putStrLn ("Sample 2: " ++ show result)

  result <- top3SumOf "data-s22e01.txt"
  putStrLn ("Part 2:   " ++ show result)

maxCaloriesOf :: FilePath -> IO Int
maxCaloriesOf file = do
  input <- getInput file
  return $ maxCalories input

top3SumOf :: FilePath -> IO Int
top3SumOf fileName = do
  input <- getInput fileName
  return $ top3Sum input

getInput :: FilePath -> IO [[Int]]
getInput file = do
  content <- readData file
  return (parseInput content)

parseInput :: String -> [[Int]]
parseInput text = toInts (split [] (lines text))
  where
    toInts :: [[String]] -> [[Int]]
    toInts = map $ map read

    split :: [String] -> [String] -> [[String]]
    split ss [] = [ss]
    split ss ("" : xs) = ss : split [] xs
    split ss (x : xs) = split (x : ss) xs

maxCalories :: [[Int]] -> Int
maxCalories = foldr (max . sum) 0

top3Sum :: [[Int]] -> Int
top3Sum = topNSum 3

topNSum :: Int -> [[Int]] -> Int
topNSum n = sum . take n . reverse . sort . map sum
