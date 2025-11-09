#include "solution.h"
#include <sstream>
#include <vector>

namespace aoc {

// Helper function to parse input
static std::vector<std::string> parse_input(const std::string& input) {
    std::vector<std::string> lines;
    std::istringstream stream(input);
    std::string line;

    while (std::getline(stream, line)) {
        if (!line.empty()) {
            lines.push_back(line);
        }
    }

    return lines;
}

int64_t solve_part1(const std::string& input) {
    auto lines = parse_input(input);

    // TODO: Implement part 1 solution
    return 0;
}

int64_t solve_part2(const std::string& input) {
    auto lines = parse_input(input);

    // TODO: Implement part 2 solution
    return 0;
}

} // namespace aoc
