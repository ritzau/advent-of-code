/** AoC 2023 Day 14: Parabolic Reflector Dish */

type Map = string[][];

function parseMap(input: string): Map {
  const lines = input.split("\n");
  const map: string[][] = [];
  for (const line of lines) {
    if (line.trim().length === 0) continue;
    map.push(Array.from(line));
  }

  return map;
}

function slideNorth(map: Map): Map {
  const stops = new Array(map[0].length).fill(0);

  for (let row = 0; row < map.length; ++row) {
    const line = map[row];
    for (let col = 0; col < line.length; ++col) {
      const element = line[col];
      if (element === "O") {
        line[col] = ".";
        map[stops[col]][col] = "O";
        ++stops[col];
      } else if (element === "#") {
        stops[col] = row + 1;
      }
    }
  }

  return map;
}

function load(map: Map): number {
  return map.reduce((sum, line, row) => {
    const multiplier = map.length - row;
    const count = line.reduce((c, elm) => (elm === "O" ? c + 1 : c), 0);
    return sum + multiplier * count;
  }, 0);
}

export function solvePart1(input: string): number {
  const map = parseMap(input);
  slideNorth(map);
  return load(map);
}

export function solvePart2(input: string): number {
  // Part 2 not implemented in original
  return 0;
}
