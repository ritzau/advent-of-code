module Main where

import Common (solvePart1)

main :: IO ()
main = do
  input <- getContents
  let result = solvePart1 (trim input)
  print result

-- Helper to trim whitespace
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (`elem` " \n\r\t")
