# Package

version       = "0.1.0"
author        = "Advent of Code"
description   = "Advent of Code 2016 Day 1 solution in Nim"
license       = "MIT"
srcDir        = "."

# Map source files (with underscores) to binary names (with dashes)
namedBin["s16e01_nim"] = "s16e01-nim"
namedBin["s16e01_nim_part1"] = "s16e01-nim-part1"
namedBin["s16e01_nim_part2"] = "s16e01-nim-part2"

# Dependencies

requires "nim >= 1.6.0"
