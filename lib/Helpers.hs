module Helpers(formatInt, printHeader, padStart, readData) where

import Data.Char(toUpper)
import Paths_AdventOfCode(getDataFileName)

printHeader :: String -> IO ()
printHeader title = do
  putStrLn ""
  putStrLn $ map toUpper title
  putStrLn $ underLine title '='
  where
    underLine (c:cs) ch = ch:underLine cs ch
    underLine _ _ = ""

readData file = do
  path <- getDataFileName file
  readFile path

formatInt :: Int -> String 
formatInt x
  | x < 0 = '-':format' (-x)
  | otherwise = format' x
  where
    format' 0 = "0"
    format' x = 
      let 
        next = x `div` 1000
        rest = x `mod` 1000
      in
        if next > 0 then
          format' next ++ "," ++ padStart 3 '0' (show rest)
        else
          show rest

padStart :: Int -> Char -> String -> String 
padStart n c s 
  | length s < n = padStart n c (c:s)
  | otherwise = s