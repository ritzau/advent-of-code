module Common (solvePart1, solvePart2) where

import Data.Char

maxJolt1 :: String -> Maybe Int
maxJolt1 (x:[]) = Just $ digitToInt x
maxJolt1 (x:xs) =
  let
    digit = digitToInt x
    tailMax = maxJolt1 xs
  in
    case tailMax of
      Just v -> Just $ max digit v
      _ -> Nothing
maxJolt1 [] = Nothing

maxJolt2 :: String -> Maybe Int
maxJolt2 (x:xs) = maxJolt 10 x (maxJolt1 xs) (maxJolt2 xs)
maxJolt2 _ = Nothing

maxJolt3 :: String -> Maybe Int
maxJolt3 (x:xs) = maxJolt 100 x (maxJolt2 xs) (maxJolt3 xs)
maxJolt3 _ = Nothing

maxJolt4 :: String -> Maybe Int
maxJolt4 (x:xs) = maxJolt 1000 x (maxJolt3 xs) (maxJolt4 xs)
maxJolt4 _ = Nothing

maxJolt5 :: String -> Maybe Int
maxJolt5 (x:xs) = maxJolt 10000 x (maxJolt4 xs) (maxJolt5 xs)
maxJolt5 _ = Nothing

maxJolt6 :: String -> Maybe Int
maxJolt6 (x:xs) = maxJolt 100000 x (maxJolt5 xs) (maxJolt6 xs)
maxJolt6 _ = Nothing

maxJolt7 :: String -> Maybe Int
maxJolt7 (x:xs) = maxJolt 1000000 x (maxJolt6 xs) (maxJolt7 xs)
maxJolt7 _ = Nothing

maxJolt8 :: String -> Maybe Int
maxJolt8 (x:xs) = maxJolt 10000000 x (maxJolt7 xs) (maxJolt8 xs)
maxJolt8 _ = Nothing

maxJolt9 :: String -> Maybe Int
maxJolt9 (x:xs) = maxJolt 100000000 x (maxJolt8 xs) (maxJolt9 xs)
maxJolt9 _ = Nothing

maxJolt10 :: String -> Maybe Int
maxJolt10 (x:xs) = maxJolt 1000000000 x (maxJolt9 xs) (maxJolt10 xs)
maxJolt10 _ = Nothing

maxJolt11 :: String -> Maybe Int
maxJolt11 (x:xs) = maxJolt 10000000000 x (maxJolt10 xs) (maxJolt11 xs)
maxJolt11 _ = Nothing

maxJolt12 :: String -> Maybe Int
maxJolt12 (x:xs) = maxJolt 100000000000 x (maxJolt11 xs) (maxJolt12 xs)
maxJolt12 _ = Nothing

maxJolt :: Int -> Char -> Maybe Int -> Maybe Int -> Maybe Int
maxJolt f x maxJoltNext skipThis =
  let
    useThis = case maxJoltNext of
      Just j -> Just $ f * (digitToInt x) + j
      _ -> Nothing
  in
    case (skipThis, useThis) of
      (Nothing, j) -> j
      (j, Nothing) -> j
      (Just sj, Just uj) -> Just $ max sj uj

sumMaxJolts2 :: [String] -> Int
sumMaxJolts2 [] = 0
sumMaxJolts2 (x:xs) =
  let
    mj = maxJolt2 x
    headJolt = case mj of
      Just j -> j
      _ -> 0
    tailJolt = sumMaxJolts2 xs
  in
    headJolt + tailJolt

sumMaxJolts12 :: [String] -> Int
sumMaxJolts12 [] = 0
sumMaxJolts12 (x:xs) =
  let
    mj = maxJolt12 x
    headJolt = case mj of
      Just j -> j
      _ -> 0
    tailJolt = sumMaxJolts12 xs
  in
    headJolt + tailJolt


solvePart1 :: String -> Int
solvePart1 = sumMaxJolts2 . lines

solvePart2 :: String -> Int
solvePart2 = sumMaxJolts12 . lines
