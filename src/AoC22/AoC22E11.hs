module AoC22E11 (monkeyInTheMiddle) where

import Data.Char (isDigit)
import Data.List (sort, stripPrefix)
import Data.Maybe (fromJust)
import Helpers (batch, printHeader, printResult, printStrResult, readData)

data Monkey = Monkey Int Operation Int Int Int
  deriving (Show)

divisableTest (Monkey _ _ d _ _) = d

data Operation = MultOp Int | AddOp Int | SquareOp
  deriving (Show)

type State = (Monkey, [Int], Int)

type NextLevelFunction = Operation -> Int -> Int

monkeyInTheMiddle :: IO ()
monkeyInTheMiddle = do
  sampleContent <- readData "data-s22e11-sample.txt"
  content <- readData "data-s22e11.txt"

  let sampleInput = parseInput sampleContent
  let input = parseInput content

  printHeader "2022 Day 11: Monkey in the Middle"

  printResult "Sample monkey business" 10605 $ monkeyBusiness 20 sampleInput

  printResult "Monkey business" 62491 $ monkeyBusiness 20 input

  printResult "Sample crazy monkey business" 2713310158 $
    monkeyBusiness' 10000 sampleInput

  printResult "Crazy monkey business" 17408399184 $
    monkeyBusiness' 10000 input

monkeyBusiness n = summarize . processRounds n next
  where
    next op lvl = nextLevel op lvl `div` 3
    summarize = product . take 2 . reverse . sort . map (\(_, _, n) -> n)

monkeyBusiness' :: Int -> [State] -> Int
monkeyBusiness' n states =
  let modulo = product $ map (\(m, _, _) -> divisableTest m) states
   in summarize $ processRounds n (next modulo) states
  where
    next modulo op lvl = nextLevel op lvl `mod` modulo
    summarize = product . take 2 . reverse . sort . map (\(_, q, n) -> n)

processRounds 0 _ states = states
processRounds n f states = processRounds (n -1) f $ processRound f states

processRound :: NextLevelFunction -> [State] -> [State]
processRound f states =
  processMonkeyId 0 states
  where
    processMonkeyId mid states
      | mid < length states = processMonkeyId (mid + 1) $ processMonkey f mid states
      | otherwise = states

processMonkey :: NextLevelFunction -> Int -> [State] -> [State]
processMonkey f iden states = processMonkey' f (states !! iden) states
  where
    processMonkey' :: NextLevelFunction -> State -> [State] -> [State]
    processMonkey' f (m, q, _) states =
      processQueue f m q states

processQueue f m@(Monkey iden op test trueTarget falseTarget) (level : ls) states =
  let newLevel = f op level
      target = if newLevel `mod` test == 0 then trueTarget else falseTarget
   in processQueue f m ls $ addItem target newLevel $ dropItem iden states
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

addItem :: Int -> Int -> [State] -> [State]
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
    parseMonkey _ = undefined

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

    parseIntList :: String -> [Int]
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