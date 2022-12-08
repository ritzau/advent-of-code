module AoC22E04 (campCleanup) where

import Data.Ix (inRange)
import Helpers (printHeader, printResult, readData, split)

type Range = (Int, Int)

type ElfPair = (Range, Range)

type Input = [ElfPair]

type Result = Int

campCleanup :: IO ()
campCleanup = do
  let sampleFile = "data-s22e04-sample.txt"
  let dataFile = "data-s22e04.txt"

  printHeader "2022 Day 4: Camp Cleanup"

  result <- countFullOverlapsOf sampleFile
  printResult "Count full overlaps of sample" 2 result

  result <- countFullOverlapsOf dataFile
  printResult "Count full overlaps" 483 result

  result <- countOverlapsOf sampleFile
  printResult "Count overlaps of sample" 4 result

  result <- countOverlapsOf dataFile
  printResult "Count overlaps" 874 result

countFullOverlapsOf :: FilePath -> IO Result
countFullOverlapsOf = process countFullOverlaps

countOverlapsOf :: FilePath -> IO Result
countOverlapsOf = process countOverlaps

process :: (Input -> r) -> FilePath -> IO r
process f file = do
  content <- readData file
  return $ (f . parseInput) content

parseInput :: String -> Input
parseInput = map processLine . lines
  where
    processLine :: String -> ElfPair
    processLine = toElfPair . toRanges . split ','
      where
        toRanges = map $ toRange' . map read . split '-'
          where
            toRange' [a, b] = (a, b)
            toRange' _ = undefined

        toElfPair [a, b] = (a, b)
        toElfPair _ = undefined

countFullOverlaps :: Input -> Result
countFullOverlaps = length . filter isFullyOverlapping

countOverlaps :: Input -> Result
countOverlaps = length . filter isOverlapping

isFullyOverlapping :: (Range, Range) -> Bool
isFullyOverlapping (a, b) =
  isOverlapping' a b || isOverlapping' b a
  where
    isOverlapping' r (low', high') =
      inRange r low' && inRange r high'

isOverlapping :: (Range, Range) -> Bool
isOverlapping (a, b) =
  isOverlapping' a b || isOverlapping' b a
  where
    isOverlapping' r (low', high') =
      inRange r low' || inRange r high'
