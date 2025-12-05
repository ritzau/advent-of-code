import std/strutils
import lib

proc main() =
  let input = stdin.readAll().strip()
  let result = solvePart2(input)
  echo result

when isMainModule:
  main()
