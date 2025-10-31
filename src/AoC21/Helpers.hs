module Helpers
  ( afterEmpty,
    batch,
    formatInt,
    padEnd,
    padStart,
    printHeader,
    printResult,
    printStrResult,
    readData,
    singleton,
    sliding,
    split,
    untilEmpty,
  )
where

import Data.Char (toUpper)
import Paths_aoc21 (getDataFileName)

afterEmpty :: [String] -> [String]
afterEmpty ("" : ls) = ls
afterEmpty (l : ls) = afterEmpty ls
afterEmpty _ = []

batch :: Int -> [a] -> [[a]]
batch _ [] = []
batch n xs =
  let (prefix, rest) = splitAt n xs
   in prefix : batch n rest

formatInt :: Int -> String
formatInt x
  | x < 0 = '-' : format' (- x)
  | otherwise = format' x
  where
    format' 0 = "0"
    format' x =
      let next = x `div` 1000
          rest = x `mod` 1000
       in if next > 0
            then format' next ++ "," ++ padStart 3 '0' (show rest)
            else show rest

padEnd :: Int -> Char -> String -> String
padEnd n char string
  | length string < n = padEnd n char (string ++ [char])
  | otherwise = string

padStart :: Int -> Char -> String -> String
padStart n char string
  | length string < n = padStart n char (char : string)
  | otherwise = string

printHeader :: String -> IO ()
printHeader title = do
  putStrLn ""
  putStrLn ""
  putStrLn $ "--- " ++ title ++ " ---"
  putStrLn ""

printResult :: Eq a => Show a => String -> a -> a -> IO ()
printResult msg expected actual = do
  let check = if actual == expected then "✅" else "❌ " ++ show expected ++ " /="

  putStrLn (padEnd 42 ' ' (msg ++ ": ") ++ check ++ " " ++ show actual)

printStrResult :: String -> String -> String -> IO ()
printStrResult msg expected actual = do
  let check = if actual == expected then "✅" else "❌ \n" ++ expected ++ " /="

  putStrLn (padEnd 42 ' ' (msg ++ ": ") ++ check ++ " \n" ++ actual)

readData :: FilePath -> IO String
readData file = do
  path <- getDataFileName file
  readFile path

sliding n a@(x : xs)
  | length xs >= n = take n a : sliding n xs
  | otherwise = [a]
sliding _ _ = []

-- split :: Char -> [Char] -> [[Char]]
-- split e = split' e []
--   where
--     split' :: Char -> [Char] -> [Char] -> [[Char]]
--     split' e r (x : xs)
--       | e == x = r : split' e [] xs
--       | otherwise = split' e (r ++ [x]) xs
--     split' e r [] = [r]

singleton x = [x]

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split sep ls =
  let (item, rest) = break (== sep) ls
  in item:split sep (drop 1 rest)
      
untilEmpty :: [String] -> [String]
untilEmpty ("" : ls) = []
untilEmpty (l : ls) = l : untilEmpty ls
untilEmpty _ = []
