module Common (solvePart1, solvePart2) where

import Data.Char
import Data.Array

maxJoltN :: Int -> String -> Maybe Int
maxJoltN maxDigits str
  | null str = Nothing
  | maxDigits <= 0 = Nothing
  | otherwise =
    let
      n = length str
      digits = map digitToInt str
      bounds = ((0, 0), (n, maxDigits))

      dp :: Array (Int, Int) (Maybe Int)
      dp = array bounds [((i, j), compute i j) | i <- [0..n], j <- [0..maxDigits]]

      compute :: Int -> Int -> Maybe Int
      compute pos remaining
        | remaining == 0 = Just 0  -- Successfully selected all digits
        | pos >= n = Nothing       -- Out of string, but still need more digits
        | otherwise =
          let
            digitsLeft = n - pos
            -- If we can't possibly select enough digits, fail early
            canFinish = digitsLeft >= remaining

            -- Option 1: Skip this digit
            skipThis = if canFinish then dp ! (pos + 1, remaining) else Nothing

            -- Option 2: Use this digit
            -- When we use a digit, it contributes: digit * 10^(remaining - 1)
            -- because we're selecting the (maxDigits - remaining + 1)th digit
            useThis = case dp ! (pos + 1, remaining - 1) of
                        Just rest -> Just $ (10 ^ (remaining - 1)) * (digits !! pos) + rest
                        Nothing -> Nothing
          in
            if not canFinish
            then Nothing
            else case (skipThis, useThis) of
              (Nothing, x) -> x
              (x, Nothing) -> x
              (Just s, Just u) -> Just $ max s u
    in
      dp ! (0, maxDigits)

sumMaxJolts :: Int -> [String] -> Int
sumMaxJolts n = sum . map (maybe 0 id . maxJoltN n)

solvePart1 :: String -> Int
solvePart1 = sumMaxJolts 2 . lines

solvePart2 :: String -> Int
solvePart2 = sumMaxJolts 12 . lines
