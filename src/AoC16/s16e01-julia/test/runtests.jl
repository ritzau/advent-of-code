using Test

@testset "AoC 2016 Day 1: No Time for a Taxicab" begin
    @testset "Part 1" begin
        # Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away
        @test S16E01.solve_part1("R2, L3") == 5

        # R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away
        @test S16E01.solve_part1("R2, R2, R2") == 2

        # R5, L5, R5, R3 leaves you 12 blocks away
        @test S16E01.solve_part1("R5, L5, R5, R3") == 12
    end

    @testset "Part 2" begin
        # R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East
        @test S16E01.solve_part2("R8, R4, R4, R8") == 4
    end
end
