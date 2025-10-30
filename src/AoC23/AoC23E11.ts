import * as readline from "node:readline";

import { createReadStream } from "fs";
import { logResult } from "./aoclib";

export async function main() {
  logResult(
    "Part 1 - sample",
    await sumOfAllExpandedPaths("AoC23E11-sample.txt"),
    374,
  );
  logResult(
    "Part 1 - input",
    await sumOfAllExpandedPaths("AoC23E11-input.txt"),
    9805264,
  );
  logResult(
    "Part 2 - sample 10",
    await sumOfAllExpandedPaths("AoC23E11-sample.txt", 10),
    1030,
  );
  logResult(
    "Part 2 - sample 100",
    await sumOfAllExpandedPaths("AoC23E11-sample.txt", 100),
    8410,
  );
  logResult(
    "Part 2 - input",
    await sumOfAllExpandedPaths("AoC23E11-input.txt", 1000000),
    779032247216,
  );
}

async function sumOfAllExpandedPaths(path: string, multiplier = 2) {
  const readable = createReadStream(path);
  const input = readline.createInterface(readable);
  const map = await parseMap(input);
  const expanded = expandMap(map, multiplier);
  return allDistances(expanded);
}

type Galaxy = {
  row: number;
  col: number;
};

async function parseMap(input: readline.Interface) {
  const map: Galaxy[] = [];

  let row = 0;
  for await (const line of input) {
    Array.from(line).forEach((ch, ind) => {
      if (ch === "#") map.push({ row, col: ind });
    });
    ++row;
  }

  return map;
}

function expandMap(map: Galaxy[], multiplier: number) {
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

function allDistances(map: Galaxy[]) {
  let sum = 0;
  for (let i = 0; i < map.length; ++i) {
    for (let j = i + 1; j < map.length; ++j) {
      sum += galaxyDistance(map[i], map[j]);
    }
  }

  return sum;
}

function galaxyDistance(a: Galaxy, b: Galaxy) {
  return Math.abs(a.row - b.row) + Math.abs(a.col - b.col);
}
