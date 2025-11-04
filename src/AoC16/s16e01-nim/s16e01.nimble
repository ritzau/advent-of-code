# Package

version       = "0.1.0"
author        = "Advent of Code"
description   = "Advent of Code 2016 Day 1 solution in Nim"
license       = "MIT"
srcDir        = "."

# Build binaries with underscores (will be renamed to dashes in Nix)
bin = @["s16e01_nim", "s16e01_nim_part1", "s16e01_nim_part2"]

# Dependencies

requires "nim >= 1.6.0"
