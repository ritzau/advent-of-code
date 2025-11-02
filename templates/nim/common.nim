## Common utilities for this day's solution
import std/strutils

proc parseInput*(input: string): seq[string] =
  input.strip().splitLines()

proc solvePart1*(input: string): int =
  let lines = parseInput(input)
  discard lines.len # Placeholder to avoid unused warning
  # TODO: Implement solution
  return 0

proc solvePart2*(input: string): int =
  let lines = parseInput(input)
  discard lines.len # Placeholder to avoid unused warning
  # TODO: Implement solution
  return 0

when isMainModule:
  import unittest

  suite "AoC Solution Tests":
    test "part1_sample_1":
      # TODO: Add sample test case
      check solvePart1("sample input") == 0

    test "part2_sample_1":
      # TODO: Add sample test case
      check solvePart2("sample input") == 0
