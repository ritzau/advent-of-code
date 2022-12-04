module AoC22E01 (calorieCounting) where

import Data.Sort (sort)
import Helpers (formatInt, printHeader, readData)

calorieCounting :: IO ()
calorieCounting = do
  printHeader "2022 Day 1: Calorie Counting"

  result <- maxCaloriesOf "data-s22e01-sample.txt"
  putStrLn ("Max calories sample: " ++ formatInt result)

  result <- maxCaloriesOf "data-s22e01.txt"
  putStrLn ("Max calories:        " ++ formatInt result)

  result <- top3SumOf "data-s22e01-sample.txt"
  putStrLn ("Top 3 total sample:  " ++ formatInt result)

  result <- top3SumOf "data-s22e01.txt"
  putStrLn ("Top 3 total:         " ++ formatInt result)

maxCaloriesOf :: FilePath -> IO Int
maxCaloriesOf = process maxCalories

top3SumOf :: FilePath -> IO Int
top3SumOf = process top3Sum

process :: ([[Int]] -> Int) -> FilePath -> IO Int
process f file = do
  input <- getInput file
  return $ f input

getInput :: FilePath -> IO [[Int]]
getInput file = do
  content <- readData file
  return $ parseInput content

parseInput :: String -> [[Int]]
parseInput = toInts . split [] . lines
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