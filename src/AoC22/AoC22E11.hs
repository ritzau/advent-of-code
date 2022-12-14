module AoC22E11 (monkeyBusiness, crazyMonkeyBusiness, parseInput) where

import Data.Char (isDigit)
import Data.List (sort, stripPrefix)
import Data.Maybe (fromJust)
import Helpers (batch, singleton, split)

data Monkey = Monkey Int Operation Int Int Int
  deriving (Show)

monkeyId :: Monkey -> Int
monkeyId (Monkey mid _ _ _ _) = mid

divisableTest :: Monkey -> Int
divisableTest (Monkey _ _ d _ _) = d

data Operation = MultOp Int | AddOp Int | SquareOp
  deriving (Show)

type State = (Monkey, [Int], Int)

inspectionCount :: State -> Int
inspectionCount (_, _, n) = n

dropItem :: Int -> [State] -> [State]
dropItem mid = map process
  where
    process s@(m, q, n)
      | mid == monkeyId m = (m, drop 1 q, n + 1)
      | otherwise = s

addItem :: Int -> Int -> [State] -> [State]
addItem mid item = map process
  where
    process s@(m, q, n)
      | mid == monkeyId m = (m, q ++ [item], n)
      | otherwise = s

monkeyBusiness :: Int -> [State] -> Int
monkeyBusiness n = summarizeMonkeyBusiness . processRounds n calcNextLevel
  where
    calcNextLevel op lvl = calculateNextLevel op lvl `div` 3

crazyMonkeyBusiness :: Int -> [State] -> Int
crazyMonkeyBusiness n states =
  let modValue = product $ map (\(m, _, _) -> divisableTest m) states
   in summarizeMonkeyBusiness $ processRounds n (calcNextLevel modValue) states
  where
    calcNextLevel modValue op lvl = calculateNextLevel op lvl `mod` modValue

summarizeMonkeyBusiness :: [State] -> Int
summarizeMonkeyBusiness =
  product . take 2 . reverse . sort . map inspectionCount

calculateNextLevel :: Operation -> Int -> Int
calculateNextLevel op level = case op of
  MultOp x -> level * x
  AddOp x -> level + x
  SquareOp -> level * level

processRounds 0 _ s0 = s0
processRounds n f s0 = processRounds (n -1) f $ processRound f
  where
    processRound f = processRound' 0 s0
      where
        processRound' mid states
          | mid < length states = processRound' (mid + 1) $ processMonkey f mid states
          | otherwise = states

        processMonkey f iden states = processMonkey' f (states !! iden) states
        processMonkey' f (m, q, _) states = processQueue f m q states

        processQueue f m@(Monkey iden op test trueTarget falseTarget) (level : ls) states =
          let newLevel = f op level
              target = if newLevel `mod` test == 0 then trueTarget else falseTarget
           in processQueue f m ls $ addItem target newLevel $ dropItem iden states
        processQueue _ _ [] states = states

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
      where
        parseOp' [op, "old"] = case op of
          "*" -> SquareOp
          "+" -> MultOp 2
          _ -> undefined
        parseOp' [op, n] = case op of
          "*" -> MultOp $ read n
          "+" -> AddOp $ read n
          _ -> undefined
        parseOp' _ = undefined

    parseDivisableTest = read . fromJust . stripPrefix "  Test: divisible by "

    parseIfTrue = read . fromJust . stripPrefix "    If true: throw to monkey"

    parseIfFalse = read . fromJust . stripPrefix "    If false: throw to monkey"

    parseIntList [] = []
    parseIntList is =
      let (number, rest) = span isDigit is
       in read number : parseIntList (dropWhile (not . isDigit) rest)
