import std/strutils
import common

proc solve(input: string): int =
  let lines = parseInput(input)
  # TODO: Implement solution
  return 0

when isMainModule:
  let input = stdin.readAll().strip()
  let result = solve(input)
  echo result
