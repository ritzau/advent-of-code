/** AoC 2023 Day 9: Mirage Maintenance */

function parseHistories(input: string): number[][] {
  const lines = input.split("\n");
  const histories: number[][] = [];
  for (const line of lines) {
    if (line.trim().length === 0) continue;
    const history = line.split(/\s+/).map((w) => parseInt(w));
    histories.push(history);
  }

  return histories;
}

function findDiff(history: number[]): number[][] {
  const stack = [history];

  while (!stack[stack.length - 1].every((x, _, a) => x == a[0])) {
    if (stack[stack.length - 1].length <= 1) {
      throw new Error("Invalid history");
    }
    const vs = stack[stack.length - 1];
    const ds = vs.slice(1).map((v, i) => v - vs[i]);
    stack.push(ds);
  }

  return stack;
}

function extrapolate(history: number[]): number {
  return findDiff(history)
    .map((vs) => vs[vs.length - 1])
    .reduce((a, b) => a + b);
}

function extrapolateBack(history: number[]): number {
  return findDiff(history)
    .map((vs) => vs[0])
    .reverse()
    .reduce((a, b) => b - a);
}

export function solvePart1(input: string): number {
  const histories = parseHistories(input);
  return histories.map((h) => extrapolate(h)).reduce((a, b) => a + b, 0);
}

export function solvePart2(input: string): number {
  const histories = parseHistories(input);
  return histories.map((h) => extrapolateBack(h)).reduce((a, b) => a + b, 0);
}
