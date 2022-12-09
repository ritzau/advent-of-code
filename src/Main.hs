module Main where

import AoC21E01 (sonarSweep)
import AoC22E01 (calorieCounting)
import AoC22E02 (rockPaperScissors)
import AoC22E03 (rucksackReorganization)
import AoC22E04 (campCleanup)
import AoC22E05 (supplyStacks)
import AoC22E06 (tuningTrouble)
import AoC22E07 (noSpaceLeftOnDevice)
import AoC22E08 (treetopTreeHouse)
import AoC22E09 (ropeBridge)

data Episode
  = S21E01
  | S22E01
  | S22E02
  | S22E03
  | S22E04
  | S22E05
  | S22E06
  | S22E07
  | S22E08
  | S22E09

runEpisode e = case e of
  S21E01 -> sonarSweep
  S22E01 -> calorieCounting
  S22E02 -> rockPaperScissors
  S22E03 -> rucksackReorganization
  S22E04 -> campCleanup
  S22E05 -> supplyStacks
  S22E06 -> tuningTrouble
  S22E07 -> noSpaceLeftOnDevice
  S22E08 -> treetopTreeHouse
  S22E09 -> ropeBridge

main :: IO ()
main =
  runEpisodes
    [ S22E01,
      S22E02,
      S22E03,
      S22E04,
      S22E05,
      S22E06,
      S22E07,
      S22E08,
      S22E09
    ]
  where
    runEpisodes (e : es) = do
      runEpisode e
      runEpisodes es
    runEpisodes _ = return ()