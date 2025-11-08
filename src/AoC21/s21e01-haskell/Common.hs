module Common
  ( solvePart1,
    solvePart2,
  )
where

-- | Parse input into list of integers
parseInput :: String -> [Int]
parseInput = map read . lines

-- | Count increases between consecutive elements
countIncreasing :: [Int] -> Int
countIncreasing xs = length $ filter isIncreasing $ pairs xs
  where
    pairs ys = zip ys (tail ys)
    isIncreasing (a, b) = a < b

-- | Create sliding windows of size n
slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs
  | length xs >= n = take n xs : slidingWindow n (tail xs)
  | otherwise = []

-- | Solve part 1: Count increases in consecutive measurements
solvePart1 :: String -> Int
solvePart1 input =
  let measurements = parseInput (trim input)
   in countIncreasing measurements

-- | Solve part 2: Count increases in sliding window sums
solvePart2 :: String -> Int
solvePart2 input =
  let measurements = parseInput (trim input)
      windows = slidingWindow 3 measurements
      sums = map sum windows
   in countIncreasing sums

-- | Helper to trim whitespace
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (`elem` " \n\r\t")
