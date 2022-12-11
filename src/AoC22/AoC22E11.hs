module AoC22E11 (cathodRayTube) where

import Data.Char (isDigit)
import Data.List (sort, stripPrefix)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)
import Helpers (batch, printHeader, printResult, printStrResult, readData)

type MonkeyInt = Integer

data Monkey = Monkey Int Operation MonkeyInt Int Int
  deriving (Show)

data Operation = MultOp MonkeyInt | AddOp MonkeyInt | SquareOp
  deriving (Show)

type State = (Monkey, [MonkeyInt], MonkeyInt)

queue (_, q, _) = q

count (_, _, n) = n

cathodRayTube :: IO ()
cathodRayTube = do
  sampleContent <- readData "data-s22e11-sample.txt"
  content <- readData "data-s22e11.txt"

  let sampleInput = parseInput sampleContent
  let input = parseInput content

  printHeader "2022 Day 11: Monkey in the Middle"

  printResult "Sample monkey business" 10605 $ monkeyBusiness 20 3 sampleInput
  printResult "Monkey business" 62491 $ monkeyBusiness 20 3 input

  putStrLn $ unlines $ monkeyBusiness' 1 1 sampleInput

monkeyBusiness n divider =
  product . take 2 . reverse . sort . map (\(_, _, n) -> n) . processRounds n divider

monkeyBusiness' n divider =
  map (\(_, q, n) -> show q) . processRounds n divider

processRounds 0 _ states = states
processRounds n divider states = processRounds (n -1) divider $ processRound divider states

processRound :: MonkeyInt -> [State] -> [State]
processRound divider states =
  processMonkeyId 0 states
  where
    processMonkeyId mid states
      | mid < length states = processMonkeyId (mid + 1) $ processMonkey divider mid states
      | otherwise = states

processMonkey :: MonkeyInt -> Int -> [State] -> [State]
processMonkey divider iden states = processMonkey' divider (states !! iden) states
  where
    processMonkey' :: MonkeyInt -> State -> [State] -> [State]
    processMonkey' divider (m, q, _) states =
      processQueue divider m q states

processQueue divider m@(Monkey iden op test trueTarget falseTarget) (level : ls) states =
  let newLevel = nextLevel op level `div` divider
      target = if newLevel `mod` test == 0 then trueTarget else falseTarget
   in processQueue divider m ls $ addItem target newLevel $ dropItem iden states
processQueue _ _ [] states = states

nextLevel op level = case op of
  MultOp x -> level * x
  AddOp x -> level + x
  SquareOp -> level * level

dropItem :: Int -> [State] -> [State]
dropItem mid = map process
  where
    process s@(m@(Monkey mid' _ _ _ _), q, n)
      | mid == mid' = (m, drop 1 q, n + 1)
      | otherwise = s

addItem :: Int -> MonkeyInt -> [State] -> [State]
addItem mid item = map process
  where
    process s@(m@(Monkey mid' _ _ _ _), q, n)
      | mid == mid' = (m, q ++ [item], n)
      | otherwise = s

parseInput :: String -> [State]
parseInput = map (parseMonkey . take 6) . split [] . lines
  where
    parseMonkey [idLine, items, op, test, ifTrue, ifFalse] =
      ( Monkey
          (parseId idLine)
          (parseOp op)
          (parseDivisableTest test)
          (parseIfTrue ifTrue)
          (parseIfFalse ifFalse),
        parseStartItems items,
        0
      )
    parseMonkey _ = (Monkey 0 (AddOp 0) 0 0 0, [], 0)

    parseId = read . takeWhile isDigit . fromJust . stripPrefix "Monkey "
    parseStartItems = parseIntList . fromJust . stripPrefix "  Starting items: "
    parseOp = parseOp' . words . fromJust . stripPrefix "  Operation: new = old "
    parseOp' [o, n]
      | n == "old" = case o of
        "*" -> SquareOp
        "+" -> MultOp 2
        _ -> undefined
      | otherwise = case o of
        "*" -> MultOp $ read n
        "+" -> AddOp $ read n
        _ -> undefined
    parseOp' _ = undefined

    parseDivisableTest = read . fromJust . stripPrefix "  Test: divisible by "

    parseIfTrue = read . fromJust . stripPrefix "    If true: throw to monkey"

    parseIfFalse = read . fromJust . stripPrefix "    If false: throw to monkey"

    parseIntList :: String -> [MonkeyInt]
    parseIntList [] = []
    parseIntList is =
      let (number, rest) = span isDigit is
       in read number : parseIntList (dropWhile (not . isDigit) rest)

    singleton x = [x]

    split :: String -> [String] -> [[String]]
    split e = split' e []
      where
        split' :: String -> [String] -> [String] -> [[String]]
        split' e r (x : xs)
          | e == x = r : split' e [] xs
          | otherwise = split' e (r ++ [x]) xs
        split' e r [] = [r]