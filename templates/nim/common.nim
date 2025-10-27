import std/strutils

proc parseInput*(input: string): seq[string] =
  input.strip().splitLines()
