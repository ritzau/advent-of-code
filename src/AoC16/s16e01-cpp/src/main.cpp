#include <chrono>
#include <iomanip>
#include <iostream>
#include <sstream>

#include "solution.h"

// Expected values for the real input
constexpr int64_t EXPECTED_PART1 = 300;
constexpr int64_t EXPECTED_PART2 = 159;

std::string read_stdin() {
    std::ostringstream buffer;
    buffer << std::cin.rdbuf();
    return buffer.str();
}

int main() {
    std::string input = read_stdin();

    std::cout << "AoC 2016 Day 1: No Time for a Taxicab\n";
    std::cout << "======================================\n";

    // Part 1
    auto start1 = std::chrono::high_resolution_clock::now();
    int64_t result1 = aoc::solve_part1(input);
    auto end1 = std::chrono::high_resolution_clock::now();
    auto duration1 = std::chrono::duration_cast<std::chrono::microseconds>(end1 - start1);
    bool pass1 = result1 == EXPECTED_PART1;

    std::cout << "Part 1: " << (pass1 ? "✅" : "❌") << " " << result1
              << " (expected: " << EXPECTED_PART1 << ") "
              << "[" << std::fixed << std::setprecision(2) << duration1.count() / 1000.0
              << " ms]\n";

    // Part 2
    auto start2 = std::chrono::high_resolution_clock::now();
    int64_t result2 = aoc::solve_part2(input);
    auto end2 = std::chrono::high_resolution_clock::now();
    auto duration2 = std::chrono::duration_cast<std::chrono::microseconds>(end2 - start2);
    bool pass2 = result2 == EXPECTED_PART2;

    std::cout << "Part 2: " << (pass2 ? "✅" : "❌") << " " << result2
              << " (expected: " << EXPECTED_PART2 << ") "
              << "[" << std::fixed << std::setprecision(2) << duration2.count() / 1000.0
              << " ms]\n";

    auto total_duration = duration1 + duration2;
    std::cout << "Total: [" << std::fixed << std::setprecision(2) << total_duration.count() / 1000.0
              << " ms]\n";

    if (pass1 && pass2) {
        std::cout << "\n🌟🌟 All tests passed!\n";
    } else {
        std::cout << "\n❌ Some tests failed\n";
        return 1;
    }

    return 0;
}
