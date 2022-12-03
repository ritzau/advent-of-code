module AoC22E03 (rucksackReorganization) where

import Data.Char (isAscii, isLower, isUpper, ord)
import qualified Data.Set as Set
import Helpers (formatInt, printHeader, readData)

type Input = [(Set.Set Char, Set.Set Char)]

type Result = Int

type BadgeInput = [[Set.Set Char]]

type BadgeResult = Int

rucksackReorganization :: IO ()
rucksackReorganization = do
  let sampleFile = "data-s22e03-sample.txt"
  let dataFile = "data-s22e03.txt"

  printHeader "2022 Day 3: Rucksack Reorganization"

  result <- sumPrioritiesOf sampleFile
  putStrLn ("Pritority of sample:      " ++ formatInt result)

  result <- sumPrioritiesOf dataFile
  putStrLn ("Priority:                 " ++ formatInt result)

  result <- sumBadgePrioritiesOf sampleFile
  putStrLn ("Badge priority of sample: " ++ formatInt result)

  result <- sumBadgePrioritiesOf dataFile
  putStrLn ("Badge priority:           " ++ formatInt result)

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
       in (Set.fromList firstHalf, Set.fromList secondHalf)

sumPriorities :: Input -> Result
sumPriorities = sum . map (scoreOfSingleton . Set.toList . intersect)
  where
    intersect (a, b) = Set.intersection a b

parseBadgeInput :: String -> BadgeInput
parseBadgeInput = batch 3 . map Set.fromList . lines
  where
    batch n xs 
      | length xs > n = take n xs:batch n (drop n xs)
      | otherwise = [xs]

sumBadgePriorities:: BadgeInput -> BadgeResult
sumBadgePriorities = sum . map processGroup
  where
    processGroup :: [Set.Set Char] -> Int
    processGroup (g:gs) = (scoreOfSingleton . Set.toList . foldr Set.intersection g) gs
    processGroup _ = undefined

scoreOfSingleton [c] = score c     
scoreOfSingleton _ = undefined

score :: Char -> Int
score c 
  | isAscii c && isLower c = 1 + ord c - ord 'a'
  | isAscii c && isUpper c = 27 + ord c - ord 'A'
  | otherwise = undefined 
