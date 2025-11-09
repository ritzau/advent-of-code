#include "solution.h"
#include <iostream>
#include <string>
#include <sstream>

// Simple test framework
int tests_run = 0;
int tests_passed = 0;

void assert_eq(int64_t actual, int64_t expected, const std::string& test_name) {
    tests_run++;
    if (actual == expected) {
        tests_passed++;
        std::cout << "✓ " << test_name << "\n";
    } else {
        std::cout << "✗ " << test_name << ": expected " << expected
                  << ", got " << actual << "\n";
    }
}

void test_part1_sample1() {
    // TODO: Add sample input
    const std::string input = R"(TODO: Add sample input)";

    int64_t result = aoc::solve_part1(input);
    int64_t expected = 0; // TODO: Add expected result

    assert_eq(result, expected, "part1_sample1");
}

void test_part2_sample1() {
    // TODO: Add sample input
    const std::string input = R"(TODO: Add sample input)";

    int64_t result = aoc::solve_part2(input);
    int64_t expected = 0; // TODO: Add expected result

    assert_eq(result, expected, "part2_sample1");
}

int main() {
    std::cout << "Running tests...\n\n";

    test_part1_sample1();
    test_part2_sample1();

    std::cout << "\n" << tests_passed << "/" << tests_run << " tests passed\n";

    return (tests_passed == tests_run) ? 0 : 1;
}
