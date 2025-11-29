using Test

# Include the library module
include("../src/lib.jl")
using .AoCSolution

@testset "AoC Solution Tests" begin
    @testset "Part 1" begin
        input = "sample input"
        @test AoCSolution.solve_part1(input) == 0
    end

    @testset "Part 2" begin
        input = "sample input"
        @test AoCSolution.solve_part2(input) == 0
    end
end
