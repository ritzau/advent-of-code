#include "solution.h"
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

// TODO: Set expected values after testing with real input
constexpr int64_t EXPECTED_PART1 = 0;
constexpr int64_t EXPECTED_PART2 = 0;

std::string read_stdin() {
    std::ostringstream buffer;
    buffer << std::cin.rdbuf();
    return buffer.str();
}

int main() {
    std::string input = read_stdin();

    // Part 1
    auto start1 = std::chrono::high_resolution_clock::now();
    int64_t result1 = aoc::solve_part1(input);
    auto end1 = std::chrono::high_resolution_clock::now();
    auto duration1 = std::chrono::duration_cast<std::chrono::microseconds>(end1 - start1);

    // Part 2
    auto start2 = std::chrono::high_resolution_clock::now();
    int64_t result2 = aoc::solve_part2(input);
    auto end2 = std::chrono::high_resolution_clock::now();
    auto duration2 = std::chrono::duration_cast<std::chrono::microseconds>(end2 - start2);

    // Display results
    std::cout << "Part 1: " << result1;
    if (result1 == EXPECTED_PART1) {
        std::cout << " ✓";
    } else {
        std::cout << " ✗ (expected " << EXPECTED_PART1 << ")";
    }
    std::cout << " (" << std::fixed << std::setprecision(2)
              << duration1.count() / 1000.0 << " ms)\n";

    std::cout << "Part 2: " << result2;
    if (result2 == EXPECTED_PART2) {
        std::cout << " ✓";
    } else {
        std::cout << " ✗ (expected " << EXPECTED_PART2 << ")";
    }
    std::cout << " (" << std::fixed << std::setprecision(2)
              << duration2.count() / 1000.0 << " ms)\n";

    // Exit with error if results don't match expected
    if (result1 != EXPECTED_PART1 || result2 != EXPECTED_PART2) {
        return 1;
    }

    return 0;
}
