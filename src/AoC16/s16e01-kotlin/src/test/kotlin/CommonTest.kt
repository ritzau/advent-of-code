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
  // Part 1 tests
  test1()
  test2()
  test3()

  // Part 2 tests
  test4()

  println("✅ All tests passed!")
}

fun test1() {
  // Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away
  val result = solvePart1("R2, L3")
  assertEquals(5, result, "Part1 sample 1: R2 L3 is 5 blocks away")
}

fun test2() {
  // R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away
  val result = solvePart1("R2, R2, R2")
  assertEquals(2, result, "Part1 sample 2: R2 R2 R2 is 2 blocks away")
}

fun test3() {
  // R5, L5, R5, R3 leaves you 12 blocks away
  val result = solvePart1("R5, L5, R5, R3")
  assertEquals(12, result, "Part1 sample 3: R5 L5 R5 R3 is 12 blocks away")
}

fun test4() {
  // R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East
  val result = solvePart2("R8, R4, R4, R8")
  assertEquals(4, result, "Part2 sample 1: R8 R4 R4 R8 first revisit is 4 blocks away")
}

fun main() {
  runTests()
}
