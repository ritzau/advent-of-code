#include <iostream>
#include <sstream>
#include <string>

#include "solution.h"

// Simple test framework
int tests_run = 0;
int tests_passed = 0;

const auto sample_input2 = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}";

const auto sample_input = R"([.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7})
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5})";

void assert_eq(int64_t actual, int64_t expected, const std::string& test_name) {
    tests_run++;
    if (actual == expected) {
        tests_passed++;
        std::cout << "✓ " << test_name << "\n";
    } else {
        std::cout << "✗ " << test_name << ": expected " << expected << ", got " << actual << "\n";
    }
}

void test_part1_sample1() {
    int64_t result = aoc::solve_part1(sample_input);
    int64_t expected = 7;

    assert_eq(result, expected, "part1_sample1");
}

void test_part2_sample1() {
    // int64_t result = aoc::solve_part2(sample_input);
    // int64_t expected = 0;  // TODO: Add expected result

    // assert_eq(result, expected, "part2_sample1");
}

int main() {
    std::cout << "Running tests...\n\n";

    test_part1_sample1();
    test_part2_sample1();

    std::cout << "\n" << tests_passed << "/" << tests_run << " tests passed\n";

    return (tests_passed == tests_run) ? 0 : 1;
}
