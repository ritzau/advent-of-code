#include "solution.h"
#include <sstream>
#include <vector>
#include <set>
#include <cmath>

namespace aoc {

enum class Direction {
    North,
    East,
    South,
    West
};

Direction turn_right(Direction dir) {
    switch (dir) {
        case Direction::North: return Direction::East;
        case Direction::East: return Direction::South;
        case Direction::South: return Direction::West;
        case Direction::West: return Direction::North;
    }
    return Direction::North;
}

Direction turn_left(Direction dir) {
    switch (dir) {
        case Direction::North: return Direction::West;
        case Direction::West: return Direction::South;
        case Direction::South: return Direction::East;
        case Direction::East: return Direction::North;
    }
    return Direction::North;
}

std::pair<int, int> delta(Direction dir) {
    switch (dir) {
        case Direction::North: return {0, 1};
        case Direction::East: return {1, 0};
        case Direction::South: return {0, -1};
        case Direction::West: return {-1, 0};
    }
    return {0, 0};
}

struct Instruction {
    char turn;
    int blocks;
};

std::vector<Instruction> parse_input(const std::string& input) {
    std::vector<Instruction> instructions;
    std::istringstream stream(input);
    std::string token;

    while (std::getline(stream, token, ',')) {
        // Trim leading whitespace
        size_t start = token.find_first_not_of(" \t\r\n");
        if (start == std::string::npos) continue;

        token = token.substr(start);

        // Trim trailing whitespace
        size_t end = token.find_last_not_of(" \t\r\n");
        if (end != std::string::npos) {
            token = token.substr(0, end + 1);
        }

        if (token.empty()) continue;

        char turn = token[0];
        int blocks = std::stoi(token.substr(1));
        instructions.push_back({turn, blocks});
    }

    return instructions;
}

int64_t solve_part1(const std::string& input) {
    auto instructions = parse_input(input);

    int x = 0;
    int y = 0;
    Direction direction = Direction::North;

    for (const auto& instruction : instructions) {
        if (instruction.turn == 'R') {
            direction = turn_right(direction);
        } else if (instruction.turn == 'L') {
            direction = turn_left(direction);
        }

        auto [dx, dy] = delta(direction);
        x += dx * instruction.blocks;
        y += dy * instruction.blocks;
    }

    return std::abs(x) + std::abs(y);
}

int64_t solve_part2(const std::string& input) {
    auto instructions = parse_input(input);

    int x = 0;
    int y = 0;
    Direction direction = Direction::North;
    std::set<std::pair<int, int>> visited;

    visited.insert({0, 0});

    for (const auto& instruction : instructions) {
        if (instruction.turn == 'R') {
            direction = turn_right(direction);
        } else if (instruction.turn == 'L') {
            direction = turn_left(direction);
        }

        auto [dx, dy] = delta(direction);

        for (int i = 0; i < instruction.blocks; i++) {
            x += dx;
            y += dy;

            if (!visited.insert({x, y}).second) {
                // Already visited this location
                return std::abs(x) + std::abs(y);
            }
        }
    }

    return 0;
}

} // namespace aoc
