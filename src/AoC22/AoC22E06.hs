module AoC22E06 (tuningTrouble) where

import Data.List (nub)
import Data.Maybe (fromJust)
import Helpers (printHeader, printResult, readData, sliding)

type Input = String

type Result = Int

tuningTrouble :: IO ()
tuningTrouble = do
  let samples =
        [ ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19),
          ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23),
          ("nppdvjthqldpwncqszvftbrmjlhg", 6, 23),
          ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29),
          ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26)
        ]

  let dataFile = "data-s22e06.txt"

  printHeader "2022 Day 6: Tuning Trouble"

  runStartOfPacketSamples samples

  result <- findStartOfPacketOf dataFile
  printResult "Input start of packet at" 1262 result

  runStartOfMessageSamples samples

  result <- findStartOfMessageOf dataFile
  printResult "Input start of message at" 3444 result
  where
    startOfPacketSamples = map (\(s, e, _) -> (s, e))
    startOfMessageSamples = map (\(s, _, e) -> (s, e))

    runStartOfPacketSamples = runSamples findStartOfPacket . startOfPacketSamples
    runStartOfMessageSamples = runSamples findStartOfMessage . startOfMessageSamples

    runSamples :: (Input -> Result) -> [(Input, Result)] -> IO ()
    runSamples = runSamples' 1
      where
        runSamples' count f ((input, expected) : ss) = do
          let actual = f input
          printResult ("Sample " ++ show count) expected actual
          runSamples' (count + 1) f ss
        runSamples' _ _ [] = return ()

findStartOfPacketOf :: FilePath -> IO Result
findStartOfPacketOf = process findStartOfPacket

findStartOfMessageOf :: FilePath -> IO Result
findStartOfMessageOf = process findStartOfMessage

process :: (Input -> r) -> FilePath -> IO r
process f file = do
  content <- readData file
  return $ f content

findStartOfPacket :: String -> Int
findStartOfPacket = findMarker 4

findStartOfMessage :: String -> Int
findStartOfMessage = findMarker 14

findMarker :: Int -> String -> Int
findMarker n = fromJust . findMarker' . zip [n ..] . sliding n
  where
    findMarker' [] = Nothing
    findMarker' ((index, m) : ms)
      | isMarker m = Just index
      | otherwise = findMarker' ms

    isMarker :: String -> Bool
    isMarker s = n == length (nub s)
