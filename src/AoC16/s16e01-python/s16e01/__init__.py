"""Advent of Code 2016 Day 1: No Time for a Taxicab"""

from .common import Direction, Instruction, parse_input

__all__ = ["Direction", "Instruction", "parse_input", "solve_part1", "solve_part2"]


def solve_part1(input_text: str) -> int:
    """Solve part 1: Find Manhattan distance to final position."""
    instructions = parse_input(input_text)

    x = 0
    y = 0
    direction = Direction.NORTH

    for instruction in instructions:
        if instruction.turn == "R":
            direction = direction.turn_right()
        elif instruction.turn == "L":
            direction = direction.turn_left()
        else:
            raise ValueError(f"Invalid turn: {instruction.turn}")

        dx, dy = direction.delta()
        x += dx * instruction.blocks
        y += dy * instruction.blocks

    return abs(x) + abs(y)


def solve_part2(input_text: str) -> int:
    """Solve part 2: Find Manhattan distance to first location visited twice."""
    instructions = parse_input(input_text)

    x = 0
    y = 0
    direction = Direction.NORTH
    visited = {(0, 0)}

    for instruction in instructions:
        if instruction.turn == "R":
            direction = direction.turn_right()
        elif instruction.turn == "L":
            direction = direction.turn_left()
        else:
            raise ValueError(f"Invalid turn: {instruction.turn}")

        dx, dy = direction.delta()

        for _ in range(instruction.blocks):
            x += dx
            y += dy

            if (x, y) in visited:
                return abs(x) + abs(y)

            visited.add((x, y))

    return 0
