/** AoC 2023 Day 13: Point of Incidence */

function* parseMap(input: string) {
  const groups = input.split("\n\n");
  for (const group of groups) {
    const lines = group.split("\n").filter((l) => l.length > 0);
    if (lines.length > 0) {
      yield lines;
    }
  }
}

function columns(map: string[]): string[] {
  const cols: string[] = [];
  for (let i = 0; i < map[0].length; ++i) {
    cols.push(Array.from(map, (l) => l[i]).join(""));
  }

  return cols;
}

function testReflection(list: string[], index: number): boolean {
  let delta = 0;

  while (0 <= index - 1 - delta && index + delta < list.length) {
    if (list[index - 1 - delta] !== list[index + delta]) {
      return false;
    }
    ++delta;
  }

  return true;
}

function findReflection(list: string[]): number {
  const reflections: number[] = [];

  let previous = list[0];
  for (let i = 1; i < list.length; ++i) {
    const current = list[i];
    if (current === previous) {
      if (testReflection(list, i)) {
        reflections.push(i);
      }
    }
    previous = current;
  }

  return reflections.length === 0 ? -1 : reflections[0];
}

function stringDistance(a: string, b: string): number {
  let mismatches = 0;
  for (let i = 0; i < a.length && mismatches <= 1; ++i) {
    if (a[i] !== b[i]) {
      ++mismatches;
    }
  }

  return mismatches;
}

type SimilarPair = { indexA: number; indexB: number; distance: number };

function testSmudgedReflection(
  n: number,
  items: SimilarPair[],
  candidate: SimilarPair
): boolean {
  let sum = 0;
  let delta = 0;
  const index = candidate.indexB;

  while (0 <= index - 1 - delta && index + delta < n) {
    const pair = items.find(
      (p) => p.indexA === index - 1 - delta && p.indexB === index + delta
    );
    if (pair === undefined) {
      return false;
    }

    sum += pair.distance;
    if (sum > 1) return false;

    ++delta;
  }

  return sum === 1;
}

function findSmudgedReflection(map: string[]): number {
  const pairs: SimilarPair[] = [];

  for (const [indexA, itemA] of map.entries()) {
    const ids = map
      .slice(indexA + 1)
      .map((itemB, indexB) => [indexA + indexB + 1, itemB] as [number, string])
      .map(([indexB, itemB]) => ({
        indexA,
        indexB,
        distance: stringDistance(itemA, itemB),
      }))
      .filter((p) => p.distance <= 1);

    pairs.push(...ids);
  }

  const reflection = pairs
    .filter((x) => x.indexB - x.indexA === 1)
    .filter((candidate) => testSmudgedReflection(map.length, pairs, candidate))
    .map((c) => c.indexB);

  return reflection.length === 0 ? -1 : reflection[0];
}

export function solvePart1(input: string): number {
  let sum = 0;
  for (const map of parseMap(input)) {
    const reflectionLine = findReflection(map);
    const reflectionColumn = findReflection(columns(map));

    if (reflectionLine !== -1) {
      sum += 100 * reflectionLine;
    }

    if (reflectionColumn !== -1) {
      sum += reflectionColumn;
    }
  }

  return sum;
}

export function solvePart2(input: string): number {
  let sum = 0;
  for (const map of parseMap(input)) {
    const reflectionLine = findSmudgedReflection(map);
    const reflectionColumn = findSmudgedReflection(columns(map));

    if (reflectionLine !== -1) {
      sum += 100 * reflectionLine;
    }

    if (reflectionColumn !== -1) {
      sum += reflectionColumn;
    }
  }

  return sum;
}
