#!/usr/bin/env kotlin

import java.io.BufferedReader
import java.io.InputStreamReader

fun parseInput(input: String): List<String> {
    return input.trim().lines()
}

fun solve(input: String): Int {
    val lines = parseInput(input)
    // TODO: Implement solution
    return 0
}

val input = BufferedReader(InputStreamReader(System.`in`)).readText()
val result = solve(input)
println(result)
