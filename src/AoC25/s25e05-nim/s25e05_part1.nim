import std/strutils
import lib

proc main() =
  let input = stdin.readAll().strip()
  let result = solvePart1(input)
  echo result

when isMainModule:
  main()
