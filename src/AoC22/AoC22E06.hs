module AoC22E06 (tuningTrouble) where

import Data.List (nub)
import Helpers (printHeader, printResult, readData, sliding)

type Input = String

type Result = Maybe Int

tuningTrouble :: IO ()
tuningTrouble = do
  let samples =
        [ ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", Just 7, Just 19),
          ("bvwbjplbgvbhsrlpgdmjqwftvncz", Just 5, Just 23),
          ("nppdvjthqldpwncqszvftbrmjlhg", Just 6, Just 23),
          ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", Just 10, Just 29),
          ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", Just 11, Just 26)
        ]

  let dataFile = "data-s22e06.txt"

  printHeader "2022 Day 6: Tuning Trouble"

  runStartOfPacketSamples samples

  result <- findStartOfPacketOf dataFile
  printResult "Input start of packet at" result (Just 1262)

  runStartOfMessageSamples samples

  result <- findStartOfMessageOf dataFile
  printResult "Input start of message at" result (Just 3444)
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
          printResult ("Sample " ++ show count) actual expected
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

findStartOfPacket :: String -> Maybe Int
findStartOfPacket = findMarker 4

findStartOfMessage :: String -> Maybe Int
findStartOfMessage = findMarker 14

findMarker :: Int -> String -> Maybe Int
findMarker n = findMarker' . zip [n ..] . sliding n
  where
    findMarker' [] = Nothing
    findMarker' ((index, m) : ms)
      | isMarker m = Just index
      | otherwise = findMarker' ms

    isMarker :: String -> Bool
    isMarker s = n == length (nub s)
