/** Common utilities for this day's solution. */

/**
 * Parse input into lines.
 */
export function parseInput(data: string): string[][] {
  return data
    .trim()
    .split("\n")
    .map((line) => Array.from(line));
}

/**
 * Solve Part 1
 */
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

/**
 * Solve Part 2
 */
export function solvePart2(input: string): number {
  const lines = parseInput(input);
  // TODO: Implement solution
  return 0;
}
