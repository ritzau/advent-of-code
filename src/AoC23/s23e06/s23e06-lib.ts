/** AoC 2023 Day 6: Wait For It */

type RaceParameters = {
  time: number;
  recordDistance: number;
};

function parseRaceParameters(input: string): RaceParameters[] {
  const lines = input.split("\n");
  const parsedLines: number[][] = [];

  for (const line of lines) {
    const values = line
      .split(/\s+/)
      .slice(1)
      .map((x) => parseInt(x));
    parsedLines.push(values);
  }

  return Array.from({ length: parsedLines[0].length }, (_, i) => ({
    time: parsedLines[0][i],
    recordDistance: parsedLines[1][i],
  }));
}

function parseRaceParameters2(input: string): RaceParameters {
  const lines = input.split("\n");
  const values: number[] = [];

  for (const line of lines) {
    const value = parseInt(line.split(/\s+/).slice(1).join(""));
    values.push(value);
  }

  return {
    time: values[0],
    recordDistance: values[1],
  };
}

function solveQuadratic(a: number, b: number, c: number): number[] {
  const discriminant = b * b - 4 * a * c;
  if (discriminant < 0) {
    return [];
  } else if (discriminant === 0) {
    return [-b / (2 * a)];
  } else {
    const root1 = (-b + Math.sqrt(discriminant)) / (2 * a);
    const root2 = (-b - Math.sqrt(discriminant)) / (2 * a);
    return [root1, root2];
  }
}

function upperBoundary(x: number): number {
  if (Number.isInteger(x)) {
    return x === 0 ? x : x + 1;
  }
  return Math.ceil(x);
}

function lowerBoundary(x: number): number {
  if (Number.isInteger(x)) {
    return x === 0 ? x : x - 1;
  }
  return Math.floor(x);
}

function solveRaces(raceParameters: RaceParameters[]): number[] {
  return raceParameters.map(({ time, recordDistance }) => {
    const roots = solveQuadratic(-1, time, -recordDistance);
    if (roots.length !== 2) {
      throw new Error("Expected exactly 2 roots");
    }
    return lowerBoundary(roots[1]) - upperBoundary(roots[0]) + 1;
  });
}

export function solvePart1(input: string): number {
  const raceParameters = parseRaceParameters(input);
  return solveRaces(raceParameters).reduce((a, b) => a * b);
}

export function solvePart2(input: string): number {
  const raceParameters = [parseRaceParameters2(input)];
  return solveRaces(raceParameters)[0];
}
