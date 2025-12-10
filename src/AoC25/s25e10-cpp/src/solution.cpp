#include "solution.h"

#include <algorithm>
#include <iostream>
#include <memory>
#include <set>
#include <sstream>
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

    struct BfsNode {
        BfsNode(std::shared_ptr<BfsNode> p, LightConfig l, int s) :
            parent(std::move(p)), lights(l), steps(s) {}

        std::shared_ptr<BfsNode> parent;
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
        if (line.empty()) continue;

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

        // std::cout << "Parsed lights: ";
        // for (bool light : config.lights) {
        //     std::cout << (light ? '#' : '.');
        // }
        // std::cout << std::endl;

        for (auto word : words) {
            // std::cout << "Word: " << word << std::endl;
            if (word.size() > 2 &&word.at(0) == '(') {
                if (word.back() != ')') {
                    throw std::runtime_error("Invalid button configuration: " + word);
                }

                word = word.substr(1, word.size() - 2);
                auto ids_str= split(word, ',');
                std::vector<int> ids;
                std::transform(ids_str.begin(), ids_str.end(), std::back_inserter(ids),
                    [](const std::string& s) { return std::stoi(s); });

                config.buttons.push_back(ids);

                // std::cout << "Parsed button: ";
                // for (int id : ids) {
                //     std::cout << id << " ";
                // }
                // std::cout << std::endl;
            }
        }
        configs.push_back(config);
    }

    // std::cout << "Total lines parsed: " << lines.size() << std::endl;

    return configs;
}

int64_t minimum_clicks(const MachineConfig& config) {
    auto visited = std::set<LightConfig>();
    auto root = std::make_shared<BfsNode>(nullptr, std::vector<bool>(config.lights.size()), 0);
    std::vector<std::shared_ptr<BfsNode>> queue;
    queue.push_back(root);

    while (!queue.empty()) {
        auto current = queue.front();
        queue.erase(queue.begin());

        if (current->lights == config.lights) {
            int64_t steps = current->steps;
            return steps;
        }

        if (visited.contains(current->lights)) {
            continue;
        }
        visited.insert(current->lights);

        for (const auto& button : config.buttons) {
            LightConfig new_lights = current->lights;
            for (int id : button) {
                if (id >= 0 && id < static_cast<int>(new_lights.size())) {
                    new_lights[id] = !new_lights[id];
                }
            }
            auto child = std::make_shared<BfsNode>(current, new_lights, current->steps + 1);
            queue.push_back(child);
        }
    }
    return -1;
}

int64_t solve_part1(const std::string& input) {
    auto machines = parse_input(input);
    // std::cout << "Number of configurations: " << machines.size() << std::endl;

    int64_t total_clicks = 0;
    for (const auto& machine : machines) {
        // std::cout << "Processing machine with " << machine.lights.size() << " lights and "
        //           << machine.buttons.size() << " buttons." << std::endl;
        int64_t clicks = minimum_clicks(machine);
        // std::cout << "Minimum clicks needed: " << clicks << std::endl;
        total_clicks += clicks;
    }

    // TODO: Implement part 1 solution
    return total_clicks;
}

int64_t solve_part2(const std::string& input) {
    auto lines = parse_input(input);

    // TODO: Implement part 2 solution
    return 0;
}

}  // namespace aoc
