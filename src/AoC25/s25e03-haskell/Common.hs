module Common
  ( solvePart1,
    solvePart2,
  )
where

import Data.Char

mx :: String -> Int
mx (x:[]) = digitToInt x
mx (x:xs) =
  let
    m = mx xs
  in
    if digitToInt x > m
    then digitToInt x
    else m
mx [] = undefined

foo :: String -> Int
foo (x:[]) = 0

foo (x:xs) = 
  let
    m = foo xs
    mmx = digitToInt x
    m1 = mx xs
    m2 = (10 * mmx) + m1
  in
    if m > m2
    then m
    else m2
    
foo _ = undefined

main = do
  let 
    haskell = foo "1296"
  putStrLn $ show haskell


-- 987654321111111
-- 811111111111119
-- 234234234234278
-- 818181911112111

solvePart1 :: String -> Int
solvePart1 = foo

solvePart2 :: String -> Int
solvePart2 input = do
  -- TODO: Implement part 2 solution
  let _ = input
  0
