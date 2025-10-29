"""Main verification script for AoC 2016 Day 1."""

import sys
import time

from s16e01 import solve_part1, solve_part2


def main():
    """Run verification for both parts with expected results."""
    input_text = sys.stdin.read()

    print("AoC 2016 Day 1: No Time for a Taxicab")
    print("=" * 38)

    # Part 1
    start = time.perf_counter()
    result1 = solve_part1(input_text)
    duration1 = time.perf_counter() - start
    expected_part1 = 300
    pass1 = result1 == expected_part1

    print(
        f"Part 1: {'✅' if pass1 else '❌'} {result1} (expected: {expected_part1}) "
        f"[{duration1*1000:.2f}ms]"
    )

    # Part 2
    start = time.perf_counter()
    result2 = solve_part2(input_text)
    duration2 = time.perf_counter() - start
    expected_part2 = 159
    pass2 = result2 == expected_part2

    print(
        f"Part 2: {'✅' if pass2 else '❌'} {result2} (expected: {expected_part2}) "
        f"[{duration2*1000:.2f}ms]"
    )

    print(f"Total: {(duration1 + duration2)*1000:.2f}ms")

    if pass1 and pass2:
        print("\n🌟🌟 All tests passed!")
        sys.exit(0)
    else:
        print("\n❌ Some tests failed")
        sys.exit(1)


if __name__ == "__main__":
    main()
