#include <iostream>
#include <string>

#include "solution.h"

// Simple test framework
int tests_run = 0;
int tests_passed = 0;

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
    // Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away
    const std::string input = "R2, L3";
    int64_t result = aoc::solve_part1(input);
    assert_eq(result, 5, "part1_sample1");
}

void test_part1_sample2() {
    // R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away
    const std::string input = "R2, R2, R2";
    int64_t result = aoc::solve_part1(input);
    assert_eq(result, 2, "part1_sample2");
}

void test_part1_sample3() {
    // R5, L5, R5, R3 leaves you 12 blocks away
    const std::string input = "R5, L5, R5, R3";
    int64_t result = aoc::solve_part1(input);
    assert_eq(result, 12, "part1_sample3");
}

void test_part2_sample1() {
    // R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East
    const std::string input = "R8, R4, R4, R8";
    int64_t result = aoc::solve_part2(input);
    assert_eq(result, 4, "part2_sample1");
}

int main() {
    std::cout << "Running tests...\n\n";

    test_part1_sample1();
    test_part1_sample2();
    test_part1_sample3();
    test_part2_sample1();

    std::cout << "\n" << tests_passed << "/" << tests_run << " tests passed\n";

    return (tests_passed == tests_run) ? 0 : 1;
}
