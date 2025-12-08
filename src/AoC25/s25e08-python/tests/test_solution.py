"""Tests for AoC solution."""

import sys
import pytest

from s25e08 import solve_part1, solve_part2

sample_input = """\
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"""


class TestPart1:
    """Tests for part 1."""

    def test_sample_1(self):
        assert solve_part1(sample_input) == 40


class TestPart2:
    """Tests for part 2."""

    def test_sample_1(self):
        assert solve_part2(sample_input) == 25272


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-v"]))
