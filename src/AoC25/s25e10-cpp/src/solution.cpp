#include "solution.h"

#include <algorithm>
#include <deque>
#include <iostream>
#include <memory>
#include <numeric>
#include <ranges>
#include <set>
#include <sstream>
#include <unordered_set>
#include <vector>

namespace aoc {

using LightConfig = std::vector<bool>;
using ButtonConfig = std::vector<std::vector<int>>;
using JoltageConfig = std::vector<int>;

struct MachineConfig {
    LightConfig lights;
    ButtonConfig buttons;
    JoltageConfig joltage;
};

// Note: std::vector<bool> doesn't have a standard hash function, so we provide one
// This implementation uses boost::hash_combine's algorithm for efficient hashing
struct LightConfigHash {
    std::size_t operator()(const LightConfig& config) const noexcept {
        std::size_t hash = config.size();
        for (std::size_t i = 0; i < config.size(); ++i) {
            if (config[i]) {
                hash ^= std::hash<std::size_t>{}(i) + 0x9e3779b9 + (hash << 6) + (hash >> 2);
            }
        }
        return hash;
    }
};

struct BfsNode {
    LightConfig lights;
    int steps;
};

// Utility function to split a string by a delimiter
static std::vector<std::string> split(const std::string& str, char delimiter) {
    std::vector<std::string> tokens;
    std::istringstream stream(str);
    std::string token;
    while (std::getline(stream, token, delimiter)) {
        tokens.push_back(token);
    }
    return tokens;
}

// Helper function to parse input
static std::vector<MachineConfig> parse_input(const std::string& input) {
    std::vector<std::string> lines;
    std::istringstream stream(input);
    std::string line;

    std::vector<MachineConfig> configs;

    while (std::getline(stream, line)) {
        if (line.empty())
            continue;

        MachineConfig config;

        auto words = split(line, ' ');
        if (words.size() < 3) {
            throw std::runtime_error("Invalid input line: " + line);
        }
        auto lights_str = words[0];
        if (lights_str.size() < 3) {
            throw std::runtime_error("Invalid lights configuration: " + lights_str);
        }
        if (lights_str.at(0) != '[' || lights_str.back() != ']') {
            throw std::runtime_error("Invalid lights format: " + lights_str);
        }
        lights_str = lights_str.substr(1, lights_str.size() - 2);
        std::transform(lights_str.begin(), lights_str.end(), std::back_inserter(config.lights),
                       [](char c) { return c == '#'; });

        for (auto word : words) {
            if (word.size() > 2 && word.at(0) == '(') {
                if (word.back() != ')') {
                    throw std::runtime_error("Invalid button configuration: " + word);
                }

                word = word.substr(1, word.size() - 2);
                auto ids_str = split(word, ',');
                std::vector<int> ids;
                std::transform(ids_str.begin(), ids_str.end(), std::back_inserter(ids),
                               [](const std::string& s) { return std::stoi(s); });

                config.buttons.push_back(ids);
            }
        }
        configs.push_back(config);
    }

    return configs;
}

int64_t minimum_clicks(const MachineConfig& config) {
    std::unordered_set<LightConfig, LightConfigHash> visited;
    std::deque<BfsNode> queue;

    queue.emplace_back(LightConfig(config.lights.size()), 0);

    while (!queue.empty()) {
        auto current = std::move(queue.front());
        queue.pop_front();

        if (current.lights == config.lights) {
            return current.steps;
        }

        if (!visited.insert(current.lights).second) {
            continue;
        }

        for (const auto& button : config.buttons) {
            auto new_lights = current.lights;
            for (int id : button) {
                if (id >= 0 && id < std::ssize(new_lights)) {
                    new_lights[id] = !new_lights[id];
                }
            }
            queue.emplace_back(std::move(new_lights), current.steps + 1);
        }
    }
    return -1;
}

int64_t solve_part1(const std::string& input) {
    auto clicks = parse_input(input) | std::views::transform([](const auto& machine) {
                      return minimum_clicks(machine);
                  });

    return std::ranges::fold_left(clicks, int64_t{0}, std::plus{});
}

int64_t solve_part2(const std::string& input) {
    auto lines = parse_input(input);

    // TODO: Implement part 2 solution
    return 0;
}

}  // namespace aoc
