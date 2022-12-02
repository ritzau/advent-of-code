module AoC22E02 (rockPaperScissors) where

import Data.Sort (sort)
import Helpers (formatInt, printHeader, readData)

data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

data TargetResult = Lose | Draw | Win
  deriving (Eq, Show)

type Round = (Move, Move)

type CorrectRound = (Move, TargetResult)

type Input = [Round]

type CorrectInput = [CorrectRound]

type Result = Int

type CorrectResult = Int

rockPaperScissors :: IO ()
rockPaperScissors = do
  printHeader "2022 Day 2: Rock Paper Scissors"

  result <- scoreOf "data-s22e02-sample.txt"
  putStrLn ("Score of sample:          " ++ formatInt result)

  result <- scoreOf "data-s22e02.txt"
  putStrLn ("Score:                    " ++ formatInt result)

  result <- correctScoreOf "data-s22e02-sample.txt"
  putStrLn ("Correct score of sample:  " ++ show result)

  result <- correctScoreOf "data-s22e02.txt"
  putStrLn ("Correct score:            " ++ formatInt result)

scoreOf :: FilePath -> IO Result
scoreOf = process score

correctScoreOf :: FilePath -> IO CorrectResult
correctScoreOf file = do
  content <- readData file
  return $ (correctScore . parseCorrectInput) content

process :: (Input -> Result) -> FilePath -> IO Result
process f file = do
  input <- getInput file
  return $ f input

getInput :: FilePath -> IO Input
getInput file = do
  content <- readData file
  return $ parseInput content

parseInput :: String -> Input
parseInput = map (toRound . words) . lines
  where
    toRound [a, b] = (toMove a, toMove b)
    toRound _ = undefined

    toMove "A" = Rock
    toMove "B" = Paper
    toMove "C" = Scissors
    toMove "X" = Rock
    toMove "Y" = Paper
    toMove "Z" = Scissors
    toMove _ = undefined

parseCorrectInput :: String -> CorrectInput
parseCorrectInput = map (toRound . words) . lines
  where
    toRound [a, b] = (toMove a, toTargetResult b)
    toRound _ = undefined

    toMove "A" = Rock
    toMove "B" = Paper
    toMove "C" = Scissors
    toMove _ = undefined

    toTargetResult "X" = Lose
    toTargetResult "Y" = Draw
    toTargetResult "Z" = Win
    toTargetResult _ = undefined

score :: Input -> Result
score = sum . map score
  where
    score m = moveScore m + roundScore m

    moveScore (_, Rock) = 1
    moveScore (_, Paper) = 2
    moveScore (_, Scissors) = 3

    roundScore (Rock, Paper) = 6
    roundScore (Paper, Scissors) = 6
    roundScore (Scissors, Rock) = 6
    roundScore (a, b)
      | a == b = 3
      | otherwise = 0

correctScore :: CorrectInput -> CorrectResult
correctScore = sum . map score
  where
    score m = (moveScore . myMove) m + roundScore m

    myMove (Rock, Win) = Paper
    myMove (Paper, Win) = Scissors
    myMove (Scissors, Win) = Rock

    myMove (m, Draw) = m

    myMove (Rock, Lose) = Scissors
    myMove (Paper, Lose) = Rock
    myMove (Scissors, Lose) = Paper

    moveScore Rock = 1
    moveScore Paper = 2
    moveScore Scissors = 3

    roundScore (_, Lose) = 0
    roundScore (_, Draw) = 3
    roundScore (_, Win) = 6
