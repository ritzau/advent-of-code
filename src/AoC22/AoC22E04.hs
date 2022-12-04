module AoC22E04 (campCleanup) where

import Helpers (formatInt, printHeader, readData, split)

data Range = Range Int Int
  deriving (Show)

type Pair = (Int, Int)

type Input = [(Range, Range)]

type Result = Int

campCleanup :: IO ()
campCleanup = do
  let sampleFile = "data-s22e04-sample.txt"
  let dataFile = "data-s22e04.txt"

  printHeader "2022 Day 4: Camp Cleanup"

  result <- countOverlapsOf sampleFile
  putStrLn ("Count overlaps of sample:        " ++ formatInt result)

  result <- countOverlapsOf dataFile
  putStrLn ("Count overlaps:                  " ++ formatInt result)

  result <- countOverlapsAtAllOf sampleFile
  putStrLn ("Count overlaps at all of sample: " ++ show result)

  result <- countOverlapsAtAllOf dataFile
  putStrLn ("Count overlaps at all:           " ++ formatInt result)

inputOf :: FilePath -> IO Input
inputOf file = do
  content <- readData file
  return $ parseInput content

countOverlapsOf :: FilePath -> IO Result
countOverlapsOf file = do
  content <- readData file
  return $ (countOverlaps . parseInput) content

countOverlapsAtAllOf :: FilePath -> IO Result
countOverlapsAtAllOf file = do
  content <- readData file
  return $ (countOverlapsAtAll . parseInput) content

parseInput :: String -> Input
parseInput = map processLine . lines
  where
    processLine :: String -> (Range, Range)
    processLine = toPair . map toSectionRange . split ','

    toSectionRange :: String -> Range
    toSectionRange = toRange . map read . split '-'

    toPair [a, b] = (a, b)
    toPair _ = undefined

    toRange [a, b] = Range a b
    toRange _ = undefined

countOverlaps :: Input -> Result
countOverlaps = length . filter isOverlapping

countOverlapsAtAll :: Input -> Result
countOverlapsAtAll = length . filter isOverlappingAtAll

isOverlapping :: (Range, Range) -> Bool
isOverlapping (a, b) = isOverlapping' a b || isOverlapping' b a
  where
    isOverlapping' (Range a b) (Range a' b') = a <= a' && b' <= b

isOverlappingAtAll :: (Range, Range) -> Bool
isOverlappingAtAll (a, b) = isOverlapping' a b || isOverlapping' b a
  where
    isOverlapping' (Range a b) (Range a' b') = 
      (a <= a' && a' <= b) || (a <= b' && b' <= b)
