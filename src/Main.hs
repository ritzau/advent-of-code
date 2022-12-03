module Main where

import AoC21E01 (sonarSweep)
import AoC22E01 (calorieCounting)
import AoC22E02 (rockPaperScissors)
import AoC22E03 (rucksackReorganization)

data Episode
  = S21E01
  | S22E01
  | S22E02
  | S22E03

runEpisode e = case e of
  S21E01 -> sonarSweep
  S22E01 -> calorieCounting
  S22E02 -> rockPaperScissors
  S22E03 -> rucksackReorganization

main :: IO ()
main =
  runEpisodes
    [ S22E01,
      S22E02,
      S22E03
    ]
  where
    runEpisodes (e : es) = do
      runEpisode e
      runEpisodes es
    runEpisodes _ = return ()