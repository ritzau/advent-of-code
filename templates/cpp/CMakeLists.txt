cmake_minimum_required(VERSION 3.20)
project(template-cpp VERSION 1.0 LANGUAGES CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Compiler flags
if(MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# Library with solve functions
add_library(solution STATIC
    src/solution.cpp
    src/solution.h
)

target_include_directories(solution PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src)

# Main verification binary
add_executable(template-cpp src/main.cpp)
target_link_libraries(template-cpp PRIVATE solution)

# Part 1 binary
add_executable(template-cpp-part1 src/part1.cpp)
target_link_libraries(template-cpp-part1 PRIVATE solution)

# Part 2 binary
add_executable(template-cpp-part2 src/part2.cpp)
target_link_libraries(template-cpp-part2 PRIVATE solution)

# Tests
enable_testing()
add_executable(test_solution tests/test_solution.cpp)
target_link_libraries(test_solution PRIVATE solution)
add_test(NAME solution_tests COMMAND test_solution)

# Install targets
install(TARGETS template-cpp template-cpp-part1 template-cpp-part2
    RUNTIME DESTINATION bin
)
