module Helpers(printHeader, readData) where

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