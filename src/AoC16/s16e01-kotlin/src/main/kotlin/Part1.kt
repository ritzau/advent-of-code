fun main() {
  val input = generateSequence(::readLine).joinToString("\n")
  val result = solvePart1(input)
  println(result)
}
