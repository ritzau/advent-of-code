module Helpers
  ( afterEmpty,
    formatInt,
    printHeader,
    padStart,
    readData,
    sliding,
    split,
    untilEmpty,
  )
where

import Data.Char (toUpper)
import Paths_AdventOfCode (getDataFileName)

afterEmpty :: [String] -> [String]
afterEmpty ("" : ls) = ls
afterEmpty (l : ls) = afterEmpty ls
afterEmpty _ = []

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

printHeader :: String -> IO ()
printHeader title = do
  putStrLn ""
  putStrLn ""
  putStrLn $ "--- " ++ title ++ " ---"
  putStrLn ""

readData :: FilePath -> IO String
readData file = do
  path <- getDataFileName file
  readFile path

padStart :: Int -> Char -> String -> String
padStart n char string
  | length string < n = padStart n char (char : string)
  | otherwise = string

sliding n a@(x : xs)
  | length xs >= n = take n a : sliding n xs
  | otherwise = [a]
sliding _ _ = []

split :: Char -> [Char] -> [[Char]]
split e = split' e []
  where
    split' :: Char -> [Char] -> [Char] -> [[Char]]
    split' e r (x : xs)
      | e == x = r : split' e [] xs
      | otherwise = split' e (r ++ [x]) xs
    split' e r [] = [r]

untilEmpty :: [String] -> [String]
untilEmpty ("" : ls) = []
untilEmpty (l : ls) = l : untilEmpty ls
untilEmpty _ = []
