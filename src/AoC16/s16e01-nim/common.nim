## Common utilities for this day's solution
import std/[strutils, sets]

type
  Direction = enum
    North
    East
    South
    West

  Instruction = object
    turn: char
    blocks: int

proc turnRight(d: Direction): Direction =
  case d
  of North: East
  of East: South
  of South: West
  of West: North

proc turnLeft(d: Direction): Direction =
  case d
  of North: West
  of West: South
  of South: East
  of East: North

proc delta(d: Direction): tuple[dx, dy: int] =
  case d
  of North: (0, 1)
  of East: (1, 0)
  of South: (0, -1)
  of West: (-1, 0)

proc parseInput*(input: string): seq[Instruction] =
  result = @[]
  let parts = input.strip().split(", ")
  for part in parts:
    if part.len > 0:
      let turn = part[0]
      let blocks = parseInt(part[1..^1])
      result.add(Instruction(turn: turn, blocks: blocks))

proc solvePart1*(input: string): int =
  let instructions = parseInput(input)

  var x = 0
  var y = 0
  var direction = North

  for instruction in instructions:
    direction = case instruction.turn
      of 'R': direction.turnRight()
      of 'L': direction.turnLeft()
      else: direction

    let (dx, dy) = direction.delta()
    x += dx * instruction.blocks
    y += dy * instruction.blocks

  return abs(x) + abs(y)

proc solvePart2*(input: string): int =
  let instructions = parseInput(input)

  var x = 0
  var y = 0
  var direction = North
  var visited = initHashSet[(int, int)]()

  visited.incl((0, 0))

  for instruction in instructions:
    direction = case instruction.turn
      of 'R': direction.turnRight()
      of 'L': direction.turnLeft()
      else: direction

    let (dx, dy) = direction.delta()

    for _ in 0..<instruction.blocks:
      x += dx
      y += dy

      if (x, y) in visited:
        return abs(x) + abs(y)
      visited.incl((x, y))

  return 0

when isMainModule:
  import unittest

  suite "AoC 2016 Day 1":
    test "part1_sample_1":
      # Following R2, L3 leaves you 2 blocks East and 3 blocks North,
      # or 5 blocks away
      check solvePart1("R2, L3") == 5

    test "part1_sample_2":
      # R2, R2, R2 leaves you 2 blocks due South of your starting
      # position, which is 2 blocks away
      check solvePart1("R2, R2, R2") == 2

    test "part1_sample_3":
      # R5, L5, R5, R3 leaves you 12 blocks away
      check solvePart1("R5, L5, R5, R3") == 12

    test "part2_sample_1":
      # R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East
      check solvePart2("R8, R4, R4, R8") == 4
