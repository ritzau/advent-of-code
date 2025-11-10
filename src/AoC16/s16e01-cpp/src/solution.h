#pragma once

#include <cstdint>
#include <string>

namespace aoc {

// Solve AoC 2016 Day 1 Part 1
// Follow instructions and calculate Manhattan distance from origin
int64_t solve_part1(const std::string& input);

// Solve AoC 2016 Day 1 Part 2
// Find first location visited twice and calculate its Manhattan distance
int64_t solve_part2(const std::string& input);

}  // namespace aoc
