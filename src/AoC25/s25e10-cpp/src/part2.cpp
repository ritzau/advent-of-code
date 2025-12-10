#include <iostream>
#include <sstream>

#include "solution.h"

std::string read_stdin() {
    std::ostringstream buffer;
    buffer << std::cin.rdbuf();
    return buffer.str();
}

int main() {
    std::string input = read_stdin();
    int64_t result = aoc::solve_part2(input);
    std::cout << result << "\n";
    return 0;
}
