fun main() {
  val input = generateSequence(::readLine).joinToString("\n")
  val result = solvePart2(input)
  println(result)
}
