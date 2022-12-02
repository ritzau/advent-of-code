module Helpers (formatInt, printHeader, padStart, readData, sliding) where

import Data.Char (toUpper)
import Paths_AdventOfCode (getDataFileName)

printHeader :: String -> IO ()
printHeader title = do
  putStrLn ""
  putStrLn ""
  putStrLn $ "--- " ++ title ++ " ---"
  putStrLn ""

readData file = do
  path <- getDataFileName file
  readFile path

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

padStart :: Int -> Char -> String -> String
padStart n c s
  | length s < n = padStart n c (c : s)
  | otherwise = s

sliding n a@(x : xs)
  | length xs >= n = take n a : sliding n xs
  | otherwise = [a]
sliding _ _ = []