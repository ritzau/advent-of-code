module AoC22E02 (rockPaperScissors) where

import Helpers (printHeader, printResult, readData)

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
  let sampleFile = "data-s22e02-sample.txt"
  let dataFile = "data-s22e02.txt"

  printHeader "2022 Day 2: Rock Paper Scissors"

  result <- scoreOf sampleFile
  printResult "Score of sample" 15 result

  result <- scoreOf dataFile
  printResult "Score" 11603 result

  result <- correctScoreOf sampleFile
  printResult "Correct score of sample" 12 result

  result <- correctScoreOf dataFile
  printResult "Correct score" 12725 result

scoreOf :: FilePath -> IO Result
scoreOf file = do
  content <- readData file
  return $ (score . parseInput) content

correctScoreOf :: FilePath -> IO CorrectResult
correctScoreOf file = do
  content <- readData file
  return $ (correctScore . parseCorrectInput) content

getInput :: FilePath -> IO Input
getInput file = do
  content <- readData file
  return $ parseInput content

parseInput :: String -> Input
parseInput = map (toRound . words) . lines
  where
    toRound [a, b] = (toMove a, toMove b)
    toRound _ = undefined

    toMove x = case x of
      "A" -> Rock
      "B" -> Paper
      "C" -> Scissors
      "X" -> Rock
      "Y" -> Paper
      "Z" -> Scissors
      _ -> undefined

parseCorrectInput :: String -> CorrectInput
parseCorrectInput = map (toRound . words) . lines
  where
    toRound [a, b] = (toMove a, toTargetResult b)
    toRound _ = undefined

    toMove x = case x of
      "A" -> Rock
      "B" -> Paper
      "C" -> Scissors
      _ -> undefined

    toTargetResult x = case x of
      "X" -> Lose
      "Y" -> Draw
      "Z" -> Win
      _ -> undefined

score :: Input -> Result
score = sum . map score
  where
    score m = moveScore m + roundScore m

    moveScore (_, m) = case m of
      Rock -> 1
      Paper -> 2
      Scissors -> 3

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

    myMove (m, Win) = case m of
      Rock -> Paper
      Paper -> Scissors
      Scissors -> Rock
    myMove (m, Draw) = m
    myMove (m, Lose) = case m of
      Rock -> Scissors
      Paper -> Rock
      Scissors -> Paper

    moveScore m = case m of
      Rock -> 1
      Paper -> 2
      Scissors -> 3

    roundScore (_, r) = case r of
      Lose -> 0
      Draw -> 3
      Win -> 6
