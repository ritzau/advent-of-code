import { createReadStream } from "node:fs";
import * as readline from "node:readline";
import { expectBlankLine, expectLine, readLine } from "../lib/utils";

export async function minLocationFromFile(
  path: string,
  start: number,
  length: number,
) {
  const readable = createReadStream(path);
  const input = readline.createInterface(readable);
  const maps = new SeedMapMap(await parseMaps(input));
  return minLocationForRange(maps, start, length);
}

function minLocationForRange(maps: SeedMapMap, start: number, length: number) {
  let minLocation = Number.POSITIVE_INFINITY;
  for (let seed = start; seed < start + length; ++seed) {
    const location = maps.lookup(seed);
    minLocation = Math.min(minLocation, location);
  }

  return minLocation;
}

export async function parseMaps(input: readline.Interface) {
  const maps = new Map<string, SeedMap>();

  while (true) {
    const map = await parseMap(input);
    if (map === undefined) break;
    maps.set(map.name, map);
  }

  return maps;
}

export async function parseSeeds(input: readline.Interface) {
  const seeds = await expectLine(input);
  await expectBlankLine(input);

  const m = seeds.match(/^seeds: (\d+(\s+\d+)*)$/);
  if (m === null) throw Error(`Parse error: ${seeds}`);
  return m[1].split(/\s+/).map((x) => parseInt(x));
}

export async function parseSeedRanges(input: readline.Interface) {
  const list = await parseSeeds(input);
  const pairs: number[][] = [];
  for (let i = 0; i < list.length; i += 2) {
    pairs.push(list.slice(i, i + 2));
  }

  return pairs;
}

export class SeedMapMap {
  private soilMap: SeedMap;
  private fertilizerMap: SeedMap;
  private waterMap: SeedMap;
  private lightMap: SeedMap;
  private temperatureMap: SeedMap;
  private humidityMap: SeedMap;
  private locationMap: SeedMap;

  constructor(readonly maps: Map<string, SeedMap>) {
    this.soilMap = maps.get("seed-to-soil map:") as SeedMap;
    this.fertilizerMap = maps.get("soil-to-fertilizer map:") as SeedMap;
    this.waterMap = maps.get("fertilizer-to-water map:") as SeedMap;
    this.lightMap = maps.get("water-to-light map:") as SeedMap;
    this.temperatureMap = maps.get("light-to-temperature map:") as SeedMap;
    this.humidityMap = maps.get("temperature-to-humidity map:") as SeedMap;
    this.locationMap = maps.get("humidity-to-location map:") as SeedMap;
  }

  lookup(seed: number) {
    const soil = this.soilMap.get(seed);
    const fertilizer = this.fertilizerMap.get(soil);
    const water = this.waterMap.get(fertilizer);
    const light = this.lightMap.get(water);
    const temperature = this.temperatureMap.get(light);
    const humidity = this.humidityMap.get(temperature);
    return this.locationMap.get(humidity);
  }
}

class MapRange {
  constructor(
    readonly destinationRangeStart: number,
    readonly sourceRangeStart: number,
    readonly rangeLength: number,
  ) {}

  get(source: number) {
    const sourceOffset = source - this.sourceRangeStart;
    if (0 <= sourceOffset && sourceOffset < this.rangeLength) {
      return this.destinationRangeStart + sourceOffset;
    }

    return undefined;
  }
}

class SeedMap {
  private ranges: MapRange[] = [];

  constructor(readonly name: string) {}

  addRange(destinationStart: number, sourceStart: number, length: number) {
    this.ranges.push(new MapRange(destinationStart, sourceStart, length));
    this.ranges.sort((a, b) => a.sourceRangeStart - b.sourceRangeStart);
  }

  get(source: number) {
    const range = this.binarySearchClosestSourceRange(source);
    return range?.get(source) ?? source;
  }

  private binarySearchClosestSourceRange(target: number): MapRange | null {
    let left = 0;
    let right = this.ranges.length - 1;
    let result: MapRange | null = null;

    while (left <= right) {
      const mid = Math.floor((left + right) / 2);
      const midRange = this.ranges[mid];

      if (midRange.sourceRangeStart <= target) {
        result = midRange; // This range is a candidate
        left = mid + 1; // Look for higher values
      } else {
        right = mid - 1; // Look for lower values
      }
    }

    return result; // The highest range where sourceRangeStart is <= target
  }

  toString() {
    return this.name;
  }
}

async function parseMap(input: readline.Interface) {
  const { done, line: name } = await readLine(input);
  if (done) return undefined;

  const map = new SeedMap(name);
  for await (const line of input) {
    if (line.length === 0) break;
    const [destinationStart, sourceStart, length] = line
      .split(/\s+/)
      .map((x) => parseInt(x));
    map.addRange(destinationStart, sourceStart, length);
  }

  return map;
}
