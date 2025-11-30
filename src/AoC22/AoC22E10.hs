module AoC22E10 (cathodRayTube) where

import Data.Maybe (fromJust)
import Helpers (batch, printHeader, printResult, printStrResult, readData)

data Instruction = Noop | AddX' Int
  deriving (Show)

cathodRayTube :: IO ()
cathodRayTube = do
  sampleContent <- readData "data-s22e10-sample.txt"
  content <- readData "data-s22e10.txt"

  let sampleInput = parseInput sampleContent
  let input = parseInput content

  printHeader "2022 Day 10: Cathode-Ray Tube"

  printResult "Sum of sample samples" 13140 $
    sumOfSamples [20, 60, 100, 140, 180, 220] sampleInput

  printResult "Sum of samples" 14780 $
    sumOfSamples [20, 60, 100, 140, 180, 220] input

  let expectedSample =
        "##..##..##..##..##..##..##..##..##..##..\n"
          ++ "###...###...###...###...###...###...###.\n"
          ++ "####....####....####....####....####....\n"
          ++ "#####.....#####.....#####.....#####.....\n"
          ++ "######......######......######......####\n"
          ++ "#######.......#######.......#######.....\n"

  let expected =
        "####.#....###..#....####..##..####.#....\n"
          ++ "#....#....#..#.#.......#.#..#....#.#....\n"
          ++ "###..#....#..#.#......#..#......#..#....\n"
          ++ "#....#....###..#.....#...#.##..#...#....\n"
          ++ "#....#....#....#....#....#..#.#....#....\n"
          ++ "####.####.#....####.####..###.####.####.\n"

  printStrResult "Sample screen render" expectedSample $
    render $ init sampleInput

  printStrResult "Screen render" expected $
    render $ init input

render :: [Instruction] -> String
render = renderScreen . execute

renderScreen :: [Int] -> String
renderScreen = unlines . map renderLine . batch 40

renderLine :: [Int] -> String
renderLine = zipWith sprite [0 ..]
  where
    sprite :: Int -> Int -> Char
    sprite a b = if abs (a - b) <= 1 then '#' else '.'

sumOfSamples :: [Int] -> [Instruction] -> Int
sumOfSamples samples =
  sum . map snd . filterSamples . signalStrength . execute
  where
    filterSamples = filter (\(c, _) -> c `elem` samples)

signalStrength :: [Int] -> [(Int, Int)]
signalStrength = zipWith (\c x -> (c, c * x)) [1 ..]

execute :: [Instruction] -> [Int]
execute = scanl (+) 1 . execute'
  where
    execute' (i : is) = execute'' i : execute' is
    execute' _ = []

    execute'' Noop = 0
    execute'' (AddX' x) = x

parseInput :: String -> [Instruction]
parseInput = concatMap processLine . lines
  where
    processLine = processLine' . words

    processLine' :: [String] -> [Instruction]
    processLine' [i, n] = toInstruction i (Just (read n))
    processLine' [i] = toInstruction i Nothing
    processLine' _ = undefined

    toInstruction :: String -> Maybe Int -> [Instruction]
    toInstruction instruction arg = case instruction of
      "noop" -> [Noop]
      "addx" -> [Noop, AddX' (fromJust arg)]
      _ -> undefined
