{-# LANGUAGE CPP #-}
module Paths_aoc22 (getDataFileName) where

-- Simple stub for Bazel builds
-- In Bazel, data files are in the runfiles directory
getDataFileName :: FilePath -> IO FilePath
getDataFileName file = return $ "src/AoC22/data/" ++ file
