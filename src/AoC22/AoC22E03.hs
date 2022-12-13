module AoC22E03 (rucksackReorganization, rucksackReorganization') where

import Data.Char (isAscii, isLower, isUpper, ord)
import Data.List (intersect, nub)
import Helpers (formatInt, printHeader, printResult, readData)

type Input = [([Char], [Char])]

type Result = Int

type BadgeInput = [[[Char]]]

type BadgeResult = Int

rucksackReorganization :: IO ()
rucksackReorganization = do
  let sampleFile = "data-s22e03-sample.txt"
  let dataFile = "data-s22e03.txt"

  printHeader "2022 Day 3: Rucksack Reorganization"

  result <- sumPrioritiesOf sampleFile
  printResult "Pritority of sample" 157 result

  result <- sumPrioritiesOf dataFile
  printResult "Pritority" 7990 result

  result <- sumBadgePrioritiesOf sampleFile
  printResult "Badge priority of sample" 70 result

  result <- sumBadgePrioritiesOf dataFile
  printResult "Badge priority" 2602 result

  result <- rucksackReorganization'
  printResult "Bonus" result 7990

sumPrioritiesOf :: FilePath -> IO Result
sumPrioritiesOf file = do
  content <- readData file
  return $ (sumPriorities . parseInput) content

sumBadgePrioritiesOf :: FilePath -> IO BadgeResult
sumBadgePrioritiesOf file = do
  content <- readData file
  return $ (sumBadgePriorities . parseBadgeInput) content

parseInput :: String -> Input
parseInput = map processLine . lines
  where
    processLine cs =
      let halfLength = length cs `div` 2
          firstHalf = take halfLength cs
          secondHalf = drop halfLength cs
       in (nub firstHalf, nub secondHalf)

sumPriorities :: Input -> Result
sumPriorities = sum . map (\(a,b) -> scoreOfSingleton $ intersect a b)

parseBadgeInput :: String -> BadgeInput
parseBadgeInput = batch 3 . map nub . lines
  where
    batch n xs
      | length xs > n = take n xs : batch n (drop n xs)
      | otherwise = [xs]

sumBadgePriorities :: BadgeInput -> BadgeResult
sumBadgePriorities = sum . map processGroup
  where
    processGroup :: [[Char]] -> Int
    processGroup (g : gs) = (scoreOfSingleton . foldr intersect g) gs
    processGroup _ = undefined

scoreOfSingleton [c] = score c
scoreOfSingleton _ = undefined

score :: Char -> Int
score c
  | isAscii c && isLower c = 1 + ord c - ord 'a'
  | isAscii c && isUpper c = 27 + ord c - ord 'A'
  | otherwise = undefined

-- Bonus

rucksackReorganization' :: IO Int
rucksackReorganization' = do
  content <- readData "data-s22e03.txt"
  return $ priority content
  where
    priority = sum . map processLine . lines
      where
        processLine l =
          let hl = length l `div` 2
           in score $ ord $ head $ nub (take hl l) `intersect` nub (drop hl l)

        score x = if x > 96 then x - 96 else x - 38
