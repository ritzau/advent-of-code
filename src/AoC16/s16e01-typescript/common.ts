/** Common utilities for AoC 2016 Day 1: No Time for a Taxicab */

enum Direction {
  NORTH = 0,
  EAST = 1,
  SOUTH = 2,
  WEST = 3,
}

interface Instruction {
  turn: "L" | "R";
  blocks: number;
}

function turnRight(dir: Direction): Direction {
  return (dir + 1) % 4;
}

function turnLeft(dir: Direction): Direction {
  return (dir + 3) % 4;
}

function getDelta(dir: Direction): [number, number] {
  switch (dir) {
    case Direction.NORTH:
      return [0, 1];
    case Direction.EAST:
      return [1, 0];
    case Direction.SOUTH:
      return [0, -1];
    case Direction.WEST:
      return [-1, 0];
  }
}

function parseInput(data: string): Instruction[] {
  return data
    .trim()
    .split(", ")
    .map((s) => ({
      turn: s[0] as "L" | "R",
      blocks: parseInt(s.slice(1), 10),
    }));
}

/**
 * Solve Part 1: Find Manhattan distance to final position
 */
export function solvePart1(input: string): number {
  const instructions = parseInput(input);

  let x = 0;
  let y = 0;
  let direction = Direction.NORTH;

  for (const instruction of instructions) {
    if (instruction.turn === "R") {
      direction = turnRight(direction);
    } else {
      direction = turnLeft(direction);
    }

    const [dx, dy] = getDelta(direction);
    x += dx * instruction.blocks;
    y += dy * instruction.blocks;
  }

  return Math.abs(x) + Math.abs(y);
}

/**
 * Solve Part 2: Find Manhattan distance to first location visited twice
 */
export function solvePart2(input: string): number {
  const instructions = parseInput(input);

  let x = 0;
  let y = 0;
  let direction = Direction.NORTH;
  const visited = new Set<string>();
  visited.add("0,0");

  for (const instruction of instructions) {
    if (instruction.turn === "R") {
      direction = turnRight(direction);
    } else {
      direction = turnLeft(direction);
    }

    const [dx, dy] = getDelta(direction);

    for (let i = 0; i < instruction.blocks; i++) {
      x += dx;
      y += dy;

      const key = `${x},${y}`;
      if (visited.has(key)) {
        return Math.abs(x) + Math.abs(y);
      }

      visited.add(key);
    }
  }

  return 0;
}
