module Main where

import qualified AoC22E01 (calorieCounting)
import qualified AoC22E02 (rockPaperScissors)
import qualified AoC22E03 (rucksackReorganization)
import qualified AoC22E04 (campCleanup)
import qualified AoC22E05 (supplyStacks)
import qualified AoC22E06 (tuningTrouble)
import qualified AoC22E07 (noSpaceLeftOnDevice)
import qualified AoC22E08 (treetopTreeHouse)
import qualified AoC22E09 (ropeBridge)
import qualified AoC22E10 (cathodRayTube)
import qualified AoC22E11 (crazyMonkeyBusiness, monkeyBusiness, parseInput)
import qualified AoC22E12 (hillClimbingAlgorithm)
import qualified AoC22E13 (distressSignal)
import Helpers (printHeader, printResult, readData)

main :: IO ()
main = do
  AoC22E01.calorieCounting
  AoC22E02.rockPaperScissors
  AoC22E03.rucksackReorganization
  AoC22E04.campCleanup
  AoC22E05.supplyStacks
  AoC22E06.tuningTrouble
  AoC22E07.noSpaceLeftOnDevice
  AoC22E08.treetopTreeHouse
  AoC22E09.ropeBridge
  AoC22E10.cathodRayTube
  s22e10MonkeyInTheMiddle
  AoC22E12.hillClimbingAlgorithm
  AoC22E13.distressSignal

s22e10MonkeyInTheMiddle = do
  printHeader "2022 Day 11: Monkey in the Middle"

  sampleContent <- readData "data-s22e11-sample.txt"
  content <- readData "data-s22e11.txt"

  let sampleInput = AoC22E11.parseInput sampleContent
  let input = AoC22E11.parseInput content

  printResult "Sample monkey business" 10605 $
    AoC22E11.monkeyBusiness 20 sampleInput

  printResult "Monkey business" 62491 $
    AoC22E11.monkeyBusiness 20 input

  printResult "Sample crazy monkey business" 2713310158 $
    AoC22E11.crazyMonkeyBusiness 10000 sampleInput

  printResult "Crazy monkey business" 17408399184 $
    AoC22E11.crazyMonkeyBusiness 10000 input
