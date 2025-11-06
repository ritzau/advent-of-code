/** AoC 2023 Day 11: Cosmic Expansion */

type Galaxy = {
  row: number;
  col: number;
};

function parseMap(input: string): Galaxy[] {
  const lines = input.split("\n");
  const map: Galaxy[] = [];

  let row = 0;
  for (const line of lines) {
    if (line.trim().length === 0) continue;
    Array.from(line).forEach((ch, ind) => {
      if (ch === "#") map.push({ row, col: ind });
    });
    ++row;
  }

  return map;
}

function expandMap(map: Galaxy[], multiplier: number): Galaxy[] {
  const rows = new Set<number>();
  const cols = new Set<number>();

  for (const g of map) {
    rows.add(g.row);
    cols.add(g.col);
  }

  const maxRow = Math.max(...rows);
  const maxCol = Math.max(...cols);

  const expandRows = Array.from({ length: maxRow }, (_, i) => i).filter(
    (x) => !rows.has(x),
  );
  const expandCols = Array.from({ length: maxCol }, (_, i) => i).filter(
    (x) => !cols.has(x),
  );

  return map.map(({ row, col }) => {
    const r =
      row +
      expandRows.reduce((a, rx) => (row >= rx ? a + (multiplier - 1) : a), 0);
    const c =
      col +
      expandCols.reduce((a, cx) => (col >= cx ? a + (multiplier - 1) : a), 0);
    return { row: r, col: c };
  });
}

function allDistances(map: Galaxy[]): number {
  let sum = 0;
  for (let i = 0; i < map.length; ++i) {
    for (let j = i + 1; j < map.length; ++j) {
      sum += galaxyDistance(map[i], map[j]);
    }
  }

  return sum;
}

function galaxyDistance(a: Galaxy, b: Galaxy): number {
  return Math.abs(a.row - b.row) + Math.abs(a.col - b.col);
}

export function solvePart1(input: string): number {
  const map = parseMap(input);
  const expanded = expandMap(map, 2);
  return allDistances(expanded);
}

export function solvePart2(
  input: string,
  multiplier: number = 1000000,
): number {
  const map = parseMap(input);
  const expanded = expandMap(map, multiplier);
  return allDistances(expanded);
}
