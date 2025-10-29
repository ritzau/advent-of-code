"""Part 2 solution runner."""

import sys

from s16e01 import solve_part2


def main():
    """Read input from stdin and print part 2 result."""
    input_text = sys.stdin.read()
    result = solve_part2(input_text)
    print(result)


if __name__ == "__main__":
    main()
