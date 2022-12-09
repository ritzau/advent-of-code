module AoC22E09 (ropeBridge) where

import Data.Char (intToDigit)
import Data.List (elemIndex, nub)
import Debug.Trace (trace, traceShowId)
import Helpers (printHeader, printResult, readData)

data Direction = GoUp | GoRight | GoDown | GoLeft
  deriving (Show)

ropeBridge :: IO ()
ropeBridge = do
  sampleContent <- readData "data-s22e09-sample.txt"
  sampleContent2 <- readData "data-s22e09-sample-2.txt"
  content <- readData "data-s22e09.txt"

  let sampleInput = parseInput sampleContent
  let sampleInput2 = parseInput sampleContent2
  let input = parseInput content

  printHeader "2022 Day 9: Rope Bridge"

  printResult "Count distinct sample positions" 13 $
    countDistinctRopePositions 2 sampleInput

  printResult "Count distinct positions" 6339 $
    countDistinctRopePositions 2 input

  printResult "Count distinct rope sample positions 1" 1 $
    countDistinctRopePositions 10 sampleInput

  printResult "Count distinct rope sample positions 2" 36 $
    countDistinctRopePositions 10 sampleInput2

  printResult "Count distinct rope positions" 2541 $
    countDistinctRopePositions 10 input

countDistinctRopePositions :: Int -> [Direction] -> Int
countDistinctRopePositions n input =
  length $ traceRope $ nub $ map last $ process (replicate n (0, 0)) input
  where
    process :: [(Int, Int)] -> [Direction] -> [[(Int, Int)]]
    process (k : ks) (d : ds) =
      let rope = moveRope (moveHead k d : ks)
       in rope : process rope ds
    process _ _ = []

    moveRope (k : k' : ks) = k : moveRope (moveKnot k k' : ks)
    moveRope [k] = [k]
    moveRope [] = undefined

    moveHead (x, y) d = case d of
      GoUp -> (x, y + 1)
      GoRight -> (x + 1, y)
      GoDown -> (x, y - 1)
      GoLeft -> (x -1, y)

    moveKnot h@(hx, hy) t@(tx, ty)
      | isTouching h t = t
      | otherwise = (tx + signum (hx - tx), ty + signum (hy - ty))

    isTouching (hx, hy) (tx, ty) =
      abs (hx - tx) <= 1 && abs (hy - ty) <= 1

traceRope r = r -- trace (renderRope 20 10 r) r

renderRope w h r =
  unlines $ reverse $ map generateLine [- h .. h]
  where
    generateLine r = [marker r c | c <- [- w .. w]]
    marker x y = maybe '.' marker' ((y, x) `elemIndex` r)
    marker' x = intToDigit (x `mod` 16)

parseInput :: String -> [Direction]
parseInput = concatMap processLine . lines
  where
    processLine = processLine' . words

    processLine' [d, n] = toMove d $ read n
    processLine' _ = undefined

    toMove d n =
      replicate n $ case d of
        "U" -> GoUp
        "R" -> GoRight
        "D" -> GoDown
        "L" -> GoLeft
        _ -> undefined