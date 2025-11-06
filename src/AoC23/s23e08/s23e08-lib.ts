/** AoC 2023 Day 8: Haunted Wasteland */

enum Direction {
  LEFT,
  RIGHT,
}

function parseMap(input: string) {
  const lines = input.split("\n");
  const direction = parseDirection(lines[0]);
  const nodes = parseNodes(lines.slice(2));
  return { direction, nodes };
}

function parseDirection(line: string): Direction[] {
  const directionMap: { [key: string]: Direction } = {
    L: Direction.LEFT,
    R: Direction.RIGHT,
  };

  return Array.from(line).map((c) => directionMap[c]);
}

function parseNodes(lines: string[]): Map<string, [string, string]> {
  const nodes = new Map<string, [string, string]>();
  for (const line of lines) {
    if (line.trim().length === 0) continue;
    const m = line.match(/^([0-9A-Z]{3}) = \(([0-9A-Z]{3}), ([0-9A-Z]{3})\)$/);
    if (m === null) throw new Error("Parse error");
    const [, label, left, right] = m;
    nodes.set(label, [left, right]);
  }

  return nodes;
}

function walkMap(
  direction: Direction[],
  nodes: Map<string, [string, string]>,
  start: string,
  predicate: (node: string) => boolean,
): number {
  let steps = 0;
  let currentNode = start;
  while (!predicate(currentNode)) {
    const step = direction[steps % direction.length];
    const node = nodes.get(currentNode) as [string, string];
    currentNode = node[step === Direction.LEFT ? 0 : 1];
    ++steps;
  }

  return steps;
}

function gcd(a: number, b: number): number {
  while (b !== 0) {
    let temp = b;
    b = a % b;
    a = temp;
  }

  return a;
}

function lcm(a: number, b: number): number {
  return Math.abs(a * b) / gcd(a, b);
}

function lcmOfArray(arr: number[]): number {
  let currentLcm = arr[0];
  for (let i = 1; i < arr.length; i++) {
    currentLcm = lcm(currentLcm, arr[i]);
  }

  return currentLcm;
}

export function solvePart1(input: string): number {
  const map = parseMap(input);
  return walkMap(map.direction, map.nodes, "AAA", (n) => n === "ZZZ");
}

export function solvePart2(input: string): number {
  const map = parseMap(input);

  const steps = Array.from(map.nodes.keys())
    .filter((n) => n.endsWith("A"))
    .map((s) => walkMap(map.direction, map.nodes, s, (n) => n.endsWith("Z")));

  return lcmOfArray(steps);
}
