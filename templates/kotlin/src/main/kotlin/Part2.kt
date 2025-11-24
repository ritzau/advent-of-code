// Advent of Code - Part 2 executable

fun main() {
    val input = generateSequence(::readLine).joinToString("\n")
    val result = solvePart2(input)
    println(result)
}
