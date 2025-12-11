#include "solution.h"

#include <algorithm>
#include <atomic>
#include <chrono>
#include <deque>
#include <iomanip>
#include <iostream>
#include <memory>
#include <mutex>
#include <numeric>
#include <queue>
#include <random>
#include <ranges>
#include <set>
#include <sstream>
#include <thread>
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

        auto joltage_str = words.back();
        if (joltage_str.size() < 3 || joltage_str.at(0) != '{' || joltage_str.back() != '}') {
            throw std::runtime_error("Invalid joltage configuration: " + joltage_str);
        }
        joltage_str = joltage_str.substr(1, joltage_str.size() - 2);
        auto joltage_values = split(joltage_str, ',');
        std::transform(joltage_values.begin(), joltage_values.end(), std::back_inserter(config.joltage),
                       [](const std::string& s) { return std::stoi(s); });

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

int64_t minimum_clicks_for_joltage(const MachineConfig& config) {
    // Create a copy of buttons and sort by length (ascending)
    auto buttons = config.buttons;
    std::ranges::sort(buttons, [](const auto& a, const auto& b) {
        return a.size() < b.size();
    });
    
    struct DfsState {
        JoltageConfig current;
        size_t button_index;
        int64_t clicks;
    };
    
    std::vector<DfsState> stack;
    stack.push_back({JoltageConfig(config.joltage.size(), 0), 0, 0});
    
    while (!stack.empty()) {
        auto state = std::move(stack.back());
        stack.pop_back();
        
        // Check if we've reached the target configuration
        if (state.current == config.joltage) {
            return state.clicks;
        }
        
        // If we've processed all buttons, this path doesn't work
        if (state.button_index >= buttons.size()) {
            continue;
        }
        
        const auto& button = buttons[state.button_index];
        
        // Calculate maximum presses without overflow
        int max_presses = std::numeric_limits<int>::max();
        for (int id : button) {
            if (id >= 0 && id < std::ssize(state.current)) {
                int diff = config.joltage[id] - state.current[id];
                max_presses = std::min(max_presses, diff);
            }
        }
        
        // Try all possible press counts from max_presses down to 0
        for (int presses = max_presses; presses >= 0; --presses) {
            auto new_state = state.current;
            for (int id : button) {
                if (id >= 0 && id < std::ssize(new_state)) {
                    new_state[id] += presses;
                }
            }
            stack.push_back({std::move(new_state), state.button_index + 1, state.clicks + presses});
        }
    }
    
    return -1;
}

int64_t solve_part2(const std::string& input) {
    auto machines = parse_input(input);
    
    // Sort machines by number of buttons (ascending)
    std::ranges::sort(machines, [](const auto& a, const auto& b) {
        return a.buttons.size() < b.buttons.size();
    });
    
    // Shuffle to keep pace consistent
    std::random_device rd;
    std::mt19937 gen(rd());
    std::ranges::shuffle(machines, gen);
    
    const size_t num_threads = std::thread::hardware_concurrency();
    const size_t total_machines = machines.size();
    
    std::cout << "Starting " << num_threads << " worker threads for " << total_machines << " machines\n";
    
    std::queue<size_t> work_queue;
    for (size_t i = 0; i < total_machines; ++i) {
        work_queue.push(i);
    }
    
    std::mutex queue_mutex;
    std::mutex output_mutex;
    std::atomic<size_t> completed{0};
    std::atomic<int64_t> total{0};
    
    auto start_time = std::chrono::high_resolution_clock::now();
    
    auto worker = [&](size_t thread_id) {
        {
            std::lock_guard<std::mutex> lock(output_mutex);
            std::cout << "Thread " << thread_id << " started\n";
        }
        
        while (true) {
            size_t index;
            {
                std::lock_guard<std::mutex> lock(queue_mutex);
                if (work_queue.empty()) {
                    return;
                }
                index = work_queue.front();
                work_queue.pop();
            }
            
            const auto& machine = machines[index];
            
            {
                std::lock_guard<std::mutex> lock(output_mutex);
                std::cout << "Thread " << thread_id << " starting machine " << index 
                          << " (buttons: " << machine.buttons.size() 
                          << ", lights: " << machine.lights.size() << ")\n";
            }
            
            auto machine_start = std::chrono::high_resolution_clock::now();
            int64_t result = minimum_clicks_for_joltage(machine);
            auto machine_end = std::chrono::high_resolution_clock::now();
            
            total += result;
            size_t done = ++completed;
            
            auto duration = std::chrono::duration_cast<std::chrono::microseconds>(machine_end - machine_start);
            auto elapsed = std::chrono::duration_cast<std::chrono::seconds>(machine_end - start_time);
            double percent = (static_cast<double>(done) / total_machines) * 100.0;
            
            // Calculate ETA
            double avg_time = static_cast<double>(elapsed.count()) / done;
            int eta_seconds = static_cast<int>(avg_time * (total_machines - done));
            int eta_minutes = eta_seconds / 60;
            eta_seconds %= 60;
            
            {
                std::lock_guard<std::mutex> lock(output_mutex);
                std::cout << "Machine " << done << "/" << total_machines
                          << " (" << std::fixed << std::setprecision(1) << percent << "%) "
                          << "(buttons: " << machine.buttons.size()
                          << ", lights: " << machine.lights.size() << "): "
                          << result << " clicks in " << duration.count() << " µs"
                          << " | ETA: " << eta_minutes << "m " << eta_seconds << "s\n";
            }
        }
    };
    
    std::vector<std::thread> threads;
    for (size_t i = 0; i < num_threads; ++i) {
        threads.emplace_back(worker, i);
    }
    
    for (auto& thread : threads) {
        thread.join();
    }
    
    return total.load();
}}  // namespace aoc
