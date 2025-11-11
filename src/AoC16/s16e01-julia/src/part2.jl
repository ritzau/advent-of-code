#!/usr/bin/env julia

# Get the directory containing this script
const SCRIPT_DIR = @__DIR__

# Include the library module
include(joinpath(SCRIPT_DIR, "lib.jl"))
using .S16E01

function main()
    # Read input from stdin
    input = read(stdin, String)
    result = solve_part2(input)
    println(result)
end

# Run main if this script is executed directly
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
