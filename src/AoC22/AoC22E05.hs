module AoC22E05 (supplyStacks) where

import Data.List (transpose)
import Helpers (afterEmpty, printHeader, readData, untilEmpty)

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
  putStrLn ("CrateMover 9000 sample end state: " ++ result)

  result <- endStateForCrateMover9000Of dataFile
  putStrLn ("CrateMover 9000 end state:        " ++ result)

  result <- endStateForCrateMover9001Of sampleFile
  putStrLn ("CrateMover 9001 sample end state: " ++ result)

  result <- endStateForCrateMover9001Of dataFile
  putStrLn ("CrateMover 9001 end state:        " ++ result)

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

    parseState = parseState' . extractStateLines
      where
        extractStateLines = drop 1 . reverse . untilEmpty
        parseState' = map (reverse . chopEnd) . (transpose . map parseStateLine)

        parseStateLine (_ : c : _ : cs) = c : parseStateLine (chopStart cs)
        parseStateLine _ = []

        chopStart (' ' : cs) = cs
        chopStart cs = cs

        chopEnd (c : cs) = case c of
          ' ' -> ""
          _ -> c : chopEnd cs
        chopEnd _ = ""

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
