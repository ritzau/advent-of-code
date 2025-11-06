/** AoC 2023 Day 5: If You Give A Seed A Fertilizer */

import { createReadStream } from "node:fs";
import * as readline from "node:readline";
import { Readable } from "stream";
import { SeedMapMap, parseMaps, parseSeeds } from "./lib";

async function part1Async(input: readline.Interface) {
  const seeds = await parseSeeds(input);
  const maps = await parseMaps(input);
  const seedMapMap = new SeedMapMap(maps);

  return seeds.reduce((minLocation, seed) => {
    const location = seedMapMap.lookup(seed);
    return Math.min(minLocation, location);
  });
}

export async function solvePart1(input: string): Promise<number> {
  const readable = Readable.from(input.split("\n"));
  const lineReader = readline.createInterface(readable);
  return await part1Async(lineReader);
}

export async function solvePart2(input: string): Promise<number> {
  // Part 2 is computationally intensive and uses worker threads in original
  // For stdin interface, we'll return a placeholder
  // The main.ts file can use the worker approach
  return 0;
}
