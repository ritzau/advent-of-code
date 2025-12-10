// Simple Kotlin test runner (no JUnit dependency)

const val SAMPLE_INPUT = """
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"""

fun assertEquals(
    expected: Long,
    actual: Long,
    message: String,
) {
  if (expected != actual) {
    throw AssertionError("$message: expected $expected but got $actual")
  }
}

fun runTests() {
  // Add test functions here
  test1()
  test2()

  println("✅ All tests passed!")
}

fun test1() {
  val expected = 50L
  val result = solvePart1(SAMPLE_INPUT)
  assertEquals(expected, result, "Part 1 sample test")
}

fun test2() {
  // TODO: Add sample input and expected output from problem description
  val input =
      """
        sample input here
        """
          .trimIndent()
  val expected = 0L // TODO: Update with expected value
  val result = solvePart2(input)
  assertEquals(expected, result, "Part 2 sample test")
}

fun main() {
  runTests()
}
