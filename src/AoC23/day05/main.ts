import { createReadStream } from "node:fs";
import * as readline from "node:readline";
import { logResult } from "../lib/utils";
import { SeedMapMap, parseMaps, parseSeedRanges, parseSeeds } from "./lib";

async function part1(path: string) {
  const readable = createReadStream(path);
  const input = readline.createInterface(readable);

  const seeds = await parseSeeds(input);
  const maps = await parseMaps(input);
  const seedMapMap = new SeedMapMap(maps);

  return seeds.reduce((minLocation, seed) => {
    const location = seedMapMap.lookup(seed);
    return Math.min(minLocation, location);
  });
}

async function part2(path: string) {
  type WorkerMinLocationFunction = (
    path: string,
    start: number,
    length: number,
  ) => Promise<number>;
  const workerMinLocation: WorkerMinLocationFunction =
    require("./worker.js");

  const readable = createReadStream(path);
  const input = readline.createInterface(readable);

  const seedRanges = await parseSeedRanges(input);
  const promises = seedRanges.map(([start, length]) =>
    workerMinLocation(path, start, length),
  );

  const locations = await Promise.all(promises);
  const minLocation = Math.min(...locations);

  return minLocation;
}

async function main(all: boolean = false) {
  logResult("Part 1 - sample", await part1("day05/sample.txt"), 35);
  logResult("Part 1 - input", await part1("../inputs/day05.txt"), 240320250);
  if (all) logResult("Part 2 - sample", await part2("day05/sample.txt"), 46);
  if (all)
    logResult("Part 2 - input", await part2("../inputs/day05.txt"), 28580589);
}

main(process.argv.includes("--all"));
