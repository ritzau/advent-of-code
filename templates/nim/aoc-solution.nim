import std/[strutils, times]
import common

proc main() =
  let input = stdin.readAll().strip()

  echo "AoC Solution"
  echo "============"

  # Part 1
  let start1 = cpuTime()
  let result1 = solvePart1(input)
  let duration1 = cpuTime() - start1
  const expectedPart1 = 0  # TODO: Set expected value
  let pass1 = result1 == expectedPart1

  let emoji1 = if pass1: "✅" else: "❌"
  echo "Part 1: ", emoji1, " ", result1, " (expected: ", expectedPart1, ") [", formatFloat(duration1 * 1000, ffDecimal, 2), "ms]"

  # Part 2
  let start2 = cpuTime()
  let result2 = solvePart2(input)
  let duration2 = cpuTime() - start2
  const expectedPart2 = 0  # TODO: Set expected value
  let pass2 = result2 == expectedPart2

  let emoji2 = if pass2: "✅" else: "❌"
  echo "Part 2: ", emoji2, " ", result2, " (expected: ", expectedPart2, ") [", formatFloat(duration2 * 1000, ffDecimal, 2), "ms]"

  echo "Total: ", formatFloat((duration1 + duration2) * 1000, ffDecimal, 2), "ms"

  if pass1 and pass2:
    echo ""
    echo "🌟🌟 All tests passed!"
  else:
    echo ""
    echo "❌ Some tests failed"
    quit(1)

when isMainModule:
  main()
