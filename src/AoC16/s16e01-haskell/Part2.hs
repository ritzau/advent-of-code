module Main where

import Common (solvePart2)

main :: IO ()
main = do
  input <- getContents
  let result = solvePart2 (trim input)
  print result

-- Helper to trim whitespace
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (`elem` " \n\r\t")
