/** Common utilities for this day's solution. */

export function parseInput(data: string): string[][] {
  return data
    .trim()
    .split("\n")
    .map((line) => Array.from(line));
}

export function solvePart1(input: string): number {
  const lines = parseInput(input);
  let sIndex = lines[0].indexOf("S");
  if (sIndex == -1) {
    throw new Error(`No start position in the first line: ${lines[0]}`);
  }

  let splitCount = 0;
  let active = [sIndex];
  let nextActive = new Set<number>();
  for (let l = 2; l < lines.length; ++l) {
    for (let a of active) {
      let symbol = lines[l][a];
      switch (symbol) {
        case ".":
          nextActive.add(a);
          break;
        case "^":
          ++splitCount;
          nextActive.add(a - 1);
          nextActive.add(a + 1);
          break;
        default:
          throw new Error(`Unexpected symbol: ${symbol}`);
      }
    }
    active = Array.from(nextActive);
    nextActive.clear();
  }

  return splitCount;
}

export function solvePart2(input: string): number {
  const lines = parseInput(input);
  const cache = lines.map((line) => Array(line.length).fill(-1));

  const sIndex = lines[0].indexOf("S");
  if (sIndex === -1) {
    throw new Error(`No start position in the first line: ${lines[0]}`);
  }

  const countPaths = (row: number, col: number): number => {
    if (row >= lines.length) return 1;
    if (cache[row][col] !== -1) return cache[row][col];
    return (cache[row][col] = calculatePaths(row, col));
  };

  const calculatePaths = (row: number, col: number): number => {
    const symbol = lines[row][col];
    switch (symbol) {
      case ".":
        return countPaths(row + 1, col);
      case "^":
        return countPaths(row + 1, col - 1) + countPaths(row + 1, col + 1);
      default:
        throw new Error(`Illegal symbol: ${symbol}`);
    }
  };

  return countPaths(1, sIndex);
}
