module AoC22E08 (treetopTreeHouse) where

import Data.Char (digitToInt)
import Data.List (break, maximumBy, nub, repeat, splitAt, transpose)
import Data.Ord (comparing)
import Helpers (printHeader, printResult, readData)

treetopTreeHouse :: IO ()
treetopTreeHouse = do
  sampleContent <- readData "data-s22e08-sample.txt"
  content <- readData "data-s22e08.txt"

  let sampleInput = parseInput sampleContent
  let input = parseInput content

  printHeader "2022 Day 8: Treetop Tree House"

  printResult "Count sample trees" 21 $ countVisibleTrees sampleInput
  printResult "Count trees" 1801 $ countVisibleTrees input

  printResult "Sample scenic score" 8 $ findMaxScenicScore sampleInput
  printResult "Scenic score" 209880 $ findMaxScenicScore input

countVisibleTrees :: [[Int]] -> Int
countVisibleTrees input =
  let indexedInput = zipWith (\i r -> zip3 (repeat i) [0 ..] r) [0 ..] input
      reversedInput = map reverse indexedInput
      transposedInput = transpose indexedInput
      reversedTransposedInput = map reverse transposedInput
      mutations =
        [ indexedInput,
          reversedInput,
          transposedInput,
          reversedTransposedInput
        ]
   in length $ nub $ concatMap (concatMap findVisibleTrees) mutations

findMaxScenicScore :: [[Int]] -> Int
findMaxScenicScore input =
  maximum
    [ scenicScore input r c
      | r <- indicies input,
        c <- indicies (head input)
    ]
  where
    indicies l = [0 .. length l - 1]

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

findVisibleTrees :: [(Int, Int, Int)] -> [(Int, Int)]
findVisibleTrees ts = findVisibleTrees' ts (-1)
  where
    findVisibleTrees' :: [(Int, Int, Int)] -> Int -> [(Int, Int)]
    findVisibleTrees' [] _ = []
    findVisibleTrees' ((row, col, height) : ts) maxHeight
      | height > maxHeight = (row, col) : findVisibleTrees' ts height
      | otherwise = findVisibleTrees' ts maxHeight

scenicScore :: [[Int]] -> Int -> Int -> Int
scenicScore input r c =
  let (row, col) = extractViews input r c
      (left, right) = viewingDistance c row
      (up, down) = viewingDistance r col
   in left * right * up * down
  where
    extractViews :: [[Int]] -> Int -> Int -> ([Int], [Int])
    extractViews input row col =
      let transposed = transpose input
       in (input !! row, transposed !! col)

    viewingDistance :: Int -> [Int] -> (Int, Int)
    viewingDistance i ts =
      let (a, b) = splitAt i ts
          a' = head b : reverse a
          b' = b
       in (viewingDistance' a', viewingDistance' b')
      where
        viewingDistance' [] = 0
        viewingDistance' (t : ts) =
          let view = takeWhile (< t) ts
              distance = length view
           in distance + if distance < length ts then 1 else 0
