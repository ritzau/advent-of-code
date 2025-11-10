#include "solution.h"
#include <iostream>
#include <sstream>

std::string read_stdin() {
    std::ostringstream buffer;
    buffer << std::cin.rdbuf();
    return buffer.str();
}

int main() {
    std::string input = read_stdin();
    int64_t result = aoc::solve_part1(input);
    std::cout << result << "\n";
    return 0;
}
