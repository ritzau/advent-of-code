module Main where

import AoC21E01

data Episode = S21E01

runEpisode S21E01 = s21E01

main :: IO ()
main = runEpisodes [S21E01]
  where
    runEpisodes (e:es) = do
      runEpisode e
      runEpisodes es
    runEpisodes _ = return ()