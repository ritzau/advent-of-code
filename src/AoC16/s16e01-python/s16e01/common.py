"""Common utilities for this day's solution."""

from dataclasses import dataclass
from enum import Enum


class Direction(Enum):
    """Cardinal directions."""

    NORTH = 0
    EAST = 1
    SOUTH = 2
    WEST = 3

    def turn_right(self) -> "Direction":
        """Turn 90 degrees clockwise."""
        return Direction((self.value + 1) % 4)

    def turn_left(self) -> "Direction":
        """Turn 90 degrees counter-clockwise."""
        return Direction((self.value - 1) % 4)

    def delta(self) -> tuple[int, int]:
        """Get the (dx, dy) movement vector for this direction."""
        match self:
            case Direction.NORTH:
                return (0, 1)
            case Direction.EAST:
                return (1, 0)
            case Direction.SOUTH:
                return (0, -1)
            case Direction.WEST:
                return (-1, 0)


@dataclass
class Instruction:
    """A single movement instruction."""

    turn: str  # 'L' or 'R'
    blocks: int


def parse_input(input_text: str) -> list[Instruction]:
    """Parse the input into a list of instructions."""
    return [
        Instruction(turn=s[0], blocks=int(s[1:]))
        for s in input_text.strip().split(", ")
    ]
