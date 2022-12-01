module Main where

import AoC21E01 (sonarSweep)
import AoC22E01 (calorieCounting)

data Episode
  = S21E01
  | S22E01

runEpisode S21E01 = sonarSweep
runEpisode S22E01 = calorieCounting

main :: IO ()
main = runEpisodes [S21E01, S22E01]
  where
    runEpisodes (e : es) = do
      runEpisode e
      runEpisodes es
    runEpisodes _ = return ()