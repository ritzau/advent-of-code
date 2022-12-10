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
