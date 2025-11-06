/** AoC 2023 Day 5: If You Give A Seed A Fertilizer */

import {
  SeedMapMap,
  parseMaps,
  parseSeedRanges,
  parseSeeds,
  minLocationForRange,
} from "./lib";

export function solvePart1(input: string): number {
  const seeds = parseSeeds(input);
  const maps = parseMaps(input);
  const seedMapMap = new SeedMapMap(maps);

  return seeds.reduce((minLocation, seed) => {
    const location = seedMapMap.lookup(seed);
    return Math.min(minLocation, location);
  }, Number.POSITIVE_INFINITY);
}

export function solvePart2(input: string): number {
  const seedRanges = parseSeedRanges(input);
  const maps = new SeedMapMap(parseMaps(input));

  let minLocation = Number.POSITIVE_INFINITY;
  for (const [start, length] of seedRanges) {
    const location = minLocationForRange(maps, start, length);
    minLocation = Math.min(minLocation, location);
  }

  return minLocation;
}
