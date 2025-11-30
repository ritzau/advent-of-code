/** AoC 2023 Day 5: If You Give A Seed A Fertilizer - Library */

export function parseSeeds(input: string): number[] {
  const lines = input.split("\n");
  const seeds = lines[0];

  const m = seeds.match(/^seeds: (\d+(\s+\d+)*)$/);
  if (m === null) throw Error(`Parse error: ${seeds}`);
  return m[1].split(/\s+/).map((x) => parseInt(x));
}

export function parseSeedRanges(input: string): number[][] {
  const list = parseSeeds(input);
  const pairs: number[][] = [];
  for (let i = 0; i < list.length; i += 2) {
    pairs.push(list.slice(i, i + 2));
  }

  return pairs;
}

export function parseMaps(input: string): Map<string, SeedMap> {
  const maps = new Map<string, SeedMap>();
  const lines = input.split("\n");

  let i = 2; // Skip seeds line and blank line
  while (i < lines.length) {
    const line = lines[i];
    if (line.trim().length === 0) {
      i++;
      continue;
    }

    const map = new SeedMap(line);
    i++;

    while (i < lines.length && lines[i].trim().length > 0) {
      const [destinationStart, sourceStart, length] = lines[i]
        .split(/\s+/)
        .map((x) => parseInt(x));
      map.addRange(destinationStart, sourceStart, length);
      i++;
    }

    maps.set(map.name, map);
  }

  return maps;
}

class MapRange {
  constructor(
    readonly destinationRangeStart: number,
    readonly sourceRangeStart: number,
    readonly rangeLength: number
  ) {}

  get(source: number): number | undefined {
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

  get(source: number): number {
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
        result = midRange;
        left = mid + 1;
      } else {
        right = mid - 1;
      }
    }

    return result;
  }

  toString() {
    return this.name;
  }
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

  lookup(seed: number): number {
    const soil = this.soilMap.get(seed);
    const fertilizer = this.fertilizerMap.get(soil);
    const water = this.waterMap.get(fertilizer);
    const light = this.lightMap.get(water);
    const temperature = this.temperatureMap.get(light);
    const humidity = this.humidityMap.get(temperature);
    return this.locationMap.get(humidity);
  }
}

export function minLocationForRange(
  maps: SeedMapMap,
  start: number,
  length: number
): number {
  let minLocation = Number.POSITIVE_INFINITY;
  for (let seed = start; seed < start + length; ++seed) {
    const location = maps.lookup(seed);
    minLocation = Math.min(minLocation, location);
  }

  return minLocation;
}
