"""Part 1 solution runner."""

import sys

from s16e01 import solve_part1


def main():
    """Read input from stdin and print part 1 result."""
    input_text = sys.stdin.read()
    result = solve_part1(input_text)
    print(result)


if __name__ == "__main__":
    main()
