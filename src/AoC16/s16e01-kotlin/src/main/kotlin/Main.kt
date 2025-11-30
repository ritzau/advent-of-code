import kotlin.system.exitProcess
import kotlin.system.measureTimeMillis

fun main() {
  val input = generateSequence(::readLine).joinToString("\n")

  println("AoC 2016 Day 1: No Time for a Taxicab")
  println("======================================")

  // Part 1
  var result1 = 0
  val duration1 = measureTimeMillis { result1 = solvePart1(input) }
  val expectedPart1 = 300
  val pass1 = result1 == expectedPart1
  val emoji1 = if (pass1) "✅" else "❌"

  println("Part 1: $emoji1 $result1 (expected: $expectedPart1) [${duration1}ms]")

  // Part 2
  var result2 = 0
  val duration2 = measureTimeMillis { result2 = solvePart2(input) }
  val expectedPart2 = 159
  val pass2 = result2 == expectedPart2
  val emoji2 = if (pass2) "✅" else "❌"

  println("Part 2: $emoji2 $result2 (expected: $expectedPart2) [${duration2}ms]")

  println("Total: ${duration1 + duration2}ms")

  if (pass1 && pass2) {
    println("\n🌟🌟 All tests passed!")
  } else {
    println("\n❌ Some tests failed")
    exitProcess(1)
  }
}
