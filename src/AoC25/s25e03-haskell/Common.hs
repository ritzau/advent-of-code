module Common (solvePart1, solvePart2) where

import Data.Array (Array, listArray, (!))
import Data.Char (digitToInt)
import Data.MemoTrie (memo2)

maxJolt :: Int -> String -> Maybe Int
maxJolt n str = go n 0
  where
    len = length str
    digits = listArray (0, len - 1) (map digitToInt str)

    go :: Int -> Int -> Maybe Int
    go = memo2 go'

    go' :: Int -> Int -> Maybe Int
    go' 1 pos
      | pos >= len = Nothing
      | otherwise = Just $ maximum [digits ! i | i <- [pos .. len - 1]]

    go' n pos
      | pos >= len = Nothing
      | otherwise = maxJolt (10 ^ (n - 1)) (digits ! pos) (go (n - 1) (pos + 1)) (go n (pos + 1))
      where
        maxJolt f digit maxJoltNext skipThis =
          max skipThis (fmap (\j -> f * digit + j) maxJoltNext)

sumMaxJolts :: Int -> [String] -> Int
sumMaxJolts n = sum . map (maybe 0 id . maxJolt n)

solvePart1 :: String -> Int
solvePart1 = sumMaxJolts 2 . lines

solvePart2 :: String -> Int
solvePart2 = sumMaxJolts 12 . lines
