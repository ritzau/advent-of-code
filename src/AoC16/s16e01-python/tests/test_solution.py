"""Tests for AoC 2016 Day 1 solution."""

from s16e01 import solve_part1, solve_part2


class TestPart1:
    """Tests for part 1."""

    def test_sample_1(self):
        """Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away."""
        assert solve_part1("R2, L3") == 5

    def test_sample_2(self):
        """R2, R2, R2 leaves you 2 blocks due South, which is 2 blocks away."""
        assert solve_part1("R2, R2, R2") == 2

    def test_sample_3(self):
        """R5, L5, R5, R3 leaves you 12 blocks away."""
        assert solve_part1("R5, L5, R5, R3") == 12


class TestPart2:
    """Tests for part 2."""

    def test_sample_1(self):
        """R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East."""
        assert solve_part2("R8, R4, R4, R8") == 4
