## Common utilities for this day's solution

import std/algorithm
import std/math
import std/sequtils
import std/strutils
import std/sugar

proc parseInput*(input: string): (seq[(int64, int64)], seq[int64]) =
  let lines = input.strip().splitLines()
  let splitIndex = findIt(lines, it == "")
  let intervallLines = lines[0 ..< splitIndex]
  let idLines = lines[(splitIndex + 1) ..^ 1]

  var intervalls = intervallLines.mapIt(
    (let parts = it.split("-");
    (parts[0].parseBiggestInt(), parts[1].parseBiggestInt());)
  )

  var ids = idLines.mapIt(it.parseBiggestInt())

  return (intervalls, ids)


proc solvePart1*(input: string): int =
  let (intervalls, ids) = parseInput(input)

  let count = ids.filter(id =>
    intervalls.any(intervall => id >= intervall[0] and id <= intervall[1])
  ).len

  return count

proc solvePart2*(input: string): int =
  let (intervalls, _) = parseInput(input)
  var merged: seq[(int64, int64)] = @[];

  for (low, high) in intervalls:
    let firstIntervall = filterIt(merged, it[0] <= low and low <= it[1])
    let lastIntervall = filterIt(merged, it[0] <= high and high <= it[1])
    merged = filterIt(merged, it[1] < low or high < it[0])

    let newLow = if firstIntervall.len > 0: firstIntervall[0][0] else: low
    let newHigh = if lastIntervall.len > 0: lastIntervall[0][1] else: high

    merged.add((newLow, newHigh))

  return merged.mapIt(it[1] - it[0] + 1).sum

when isMainModule:
  import unittest

  let sampleInput = """
3-5
10-14
16-20
12-18

1
5
8
11
17
32
"""

  suite "AoC Solution Tests":
    test "part1":
      check solvePart1(sampleInput) == 3

    test "part2":
      check solvePart2(sampleInput) == 14
