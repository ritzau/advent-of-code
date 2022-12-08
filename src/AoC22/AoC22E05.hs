module AoC22E05 (supplyStacks) where

import Data.Char (isSpace)
import Data.List (transpose)
import Helpers (afterEmpty, printHeader, printResult, readData, untilEmpty)

type State = [[Char]]

type Move = (Int, Int, Int)

data Input = Input State [Move]
  deriving (Show)

type Result = String

supplyStacks :: IO ()
supplyStacks = do
  let sampleFile = "data-s22e05-sample.txt"
  let dataFile = "data-s22e05.txt"

  printHeader "2022 Day 5: Supply Stacks"

  result <- endStateForCrateMover9000Of sampleFile
  printResult "CrateMover 9000 sample end state" "CMZ" result 

  result <- endStateForCrateMover9000Of dataFile
  printResult "CrateMover 9000 end state" "VRWBSFZWM" result 

  result <- endStateForCrateMover9001Of sampleFile
  printResult "CrateMover 9001 sample end state" "MCD" result 

  result <- endStateForCrateMover9001Of dataFile
  printResult "CrateMover 9001 end state" "RBTWJWMCF" result 

endStateForCrateMover9000Of :: FilePath -> IO Result
endStateForCrateMover9000Of = process endStateForCrateMover9000

endStateForCrateMover9001Of :: FilePath -> IO Result
endStateForCrateMover9001Of = process endStateForCrateMover9001

process :: (Input -> State) -> FilePath -> IO Result
process f file = do
  content <- readData file
  return $ (map head . f . parseInput) content

parseInput :: String -> Input
parseInput = parseInput' . lines
  where
    parseInput' ls = Input (parseState ls) (parseMoves ls)

    parseState :: [String] -> [String]
    parseState = reverseAndTrim . transpose . removeRedundantChars . getStateLines
      where
        getStateLines = drop 1 . reverse . takeWhile (not . null)

        removeRedundantChars = map (removeRedundantChars' . drop 1)
        removeRedundantChars' [] = []
        removeRedundantChars' (c : cs) = c : removeRedundantChars' (drop 3 cs)

        reverseAndTrim = map (dropWhile isSpace . reverse)

    parseMoves = map parseMove . afterEmpty
      where
        parseMove :: String -> Move
        parseMove l =
          let ws = words l
              n = read (ws !! 1)
              from = read (ws !! 3)
              to = read (ws !! 5)
           in (n, from, to)

endStateForCrateMover9000 :: Input -> State
endStateForCrateMover9000 (Input s []) = s
endStateForCrateMover9000 (Input state (m : ms)) =
  endStateForCrateMover9000 (Input (moveCrate state m) ms)
  where
    moveCrate :: State -> Move -> State
    moveCrate state (0, _, _) = state
    moveCrate state m@(n, from, to) = moveCrate (move' state m) (n - 1, from, to)
      where
        move' :: State -> Move -> State
        move' state (n, from, to) =
          let fromStack = state !! (from - 1)
              crate = head fromStack
           in zipWith (processStack crate) [1 ..] state
          where
            processStack :: Char -> Int -> String -> String
            processStack movedCrate index stack
              | index == from = drop 1 stack
              | index == to = movedCrate : stack
              | otherwise = stack

endStateForCrateMover9001 :: Input -> State
endStateForCrateMover9001 (Input s []) = s
endStateForCrateMover9001 (Input s (m : ms)) =
  endStateForCrateMover9001 (Input (moveCrates s m) ms)
  where
    moveCrates :: State -> Move -> State
    moveCrates state (n, from, to) =
      let fromStack = state !! (from - 1)
          crates = take n fromStack
       in zipWith (processStack crates) [1 ..] state
      where
        processStack :: String -> Int -> String -> String
        processStack movedCrates index stack
          | index == from = drop (length movedCrates) stack
          | index == to = movedCrates ++ stack
          | otherwise = stack
