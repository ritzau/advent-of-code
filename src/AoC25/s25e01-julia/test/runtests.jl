using Test

# Include the library module
include("../src/lib.jl")
using .AoCSolution

@testset "AoC Solution Tests" begin
    input = """
        L68
        L30
        R48
        L5
        R60
        L55
        L1
        L99
        R14
        L82
        """

    @testset "Part 1" begin
        @test AoCSolution.solve_part1(input) == 3
    end

    @testset "Part 2" begin
        @test AoCSolution.solve_part2(input) == 6
    end
end
