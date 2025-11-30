// Simple Kotlin test runner (no JUnit dependency)

fun assertEquals(
    expected: Int,
    actual: Int,
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
  // TODO: Add sample input and expected output from problem description
  val input =
      """
        sample input here
        """
          .trimIndent()
  val expected = 0 // TODO: Update with expected value
  val result = solvePart1(input)
  assertEquals(expected, result, "Part 1 sample test")
}

fun test2() {
  // TODO: Add sample input and expected output from problem description
  val input =
      """
        sample input here
        """
          .trimIndent()
  val expected = 0 // TODO: Update with expected value
  val result = solvePart2(input)
  assertEquals(expected, result, "Part 2 sample test")
}

fun main() {
  runTests()
}
