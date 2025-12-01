#!/usr/bin/env julia

# Get the directory containing this script
const SCRIPT_DIR = @__DIR__

# Include the library module
include(joinpath(SCRIPT_DIR, "lib.jl"))
using .AoCSolution

function main()
    # Read input from stdin
    input = read(stdin, String)

    println("Advent of Code - Day X")
    println("======================")

    # Part 1
    start_time = time()
    result1 = solve_part1(input)
    duration1 = time() - start_time
    expected_part1 = 980
    pass1 = result1 == expected_part1

    emoji1 = pass1 ? "✅" : "❌"
    println("Part 1: $emoji1 $result1 (expected: $expected_part1) [$(round(duration1 * 1000, digits=2))ms]")

    # Part 2
    start_time = time()
    result2 = solve_part2(input)
    duration2 = time() - start_time
    expected_part2 = 5961
    pass2 = result2 == expected_part2

    emoji2 = pass2 ? "✅" : "❌"
    println("Part 2: $emoji2 $result2 (expected: $expected_part2) [$(round(duration2 * 1000, digits=2))ms]")

    println("Total: $(round((duration1 + duration2) * 1000, digits=2))ms")

    if pass1 && pass2
        println("\n🌟🌟 All tests passed!")
        exit(0)
    else
        println("\n❌ Some tests failed")
        exit(1)
    end
end

# Run main if this script is executed directly
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
