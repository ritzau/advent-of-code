/** AoC 2023 Day 2: Cube Conundrum */

import { open } from "node:fs/promises";
import { Interface } from "readline";
import { asyncSum } from "../lib/utils";

type CubeSet = {
  red: number;
  green: number;
  blue: number;
};

type CubeGame = {
  game: number;
  sets: CubeSet[];
};

async function readLinesFromFile(path: string): Promise<Interface> {
  const file = await open(path);
  return file.readLines();
}

async function* possibleGames(asyncLines: Interface) {
  const bag: CubeSet = { red: 12, green: 13, blue: 14 };

  for await (const line of asyncLines) {
    if (line.length === 0) continue;

    const game = parseLine(line);
    if (!possible(bag, game)) continue;

    yield game.game;
  }
}

async function* minimumPower(asyncLines: Interface) {
  const bag: CubeSet = { red: 12, green: 13, blue: 14 };

  for await (const line of asyncLines) {
    if (line.length === 0) continue;

    const game = parseLine(line);
    const min = minimumSet(game);
    yield min.red * min.green * min.blue;
  }
}

function part1Sync(input: string[]): number {
  const bag: CubeSet = { red: 12, green: 13, blue: 14 };

  return input
    .filter((l) => l.length > 0)
    .map(parseLine)
    .filter((g) => possible(bag, g))
    .map(({ game }) => game)
    .reduce((a, b) => a + b);
}

function possible(bag: CubeSet, { game, sets }: CubeGame): boolean {
  return sets.every(
    ({ red, green, blue }) =>
      red <= bag.red && green <= bag.green && blue <= bag.blue,
  );
}

function part2Sync(input: string[]) {
  return input
    .filter((l) => l.length > 0)
    .map(parseLine)
    .map(minimumSet)
    .map(({ red, green, blue }) => red * green * blue)
    .reduce((a, b) => a + b);
}

function minimumSet({ game, sets }: CubeGame): CubeSet {
  const minumum = sets.pop();
  if (minumum === undefined) {
    throw new Error("No sets in game");
  }

  for (const set of sets) {
    minumum.red = Math.max(minumum.red, set.red);
    minumum.green = Math.max(minumum.green, set.green);
    minumum.blue = Math.max(minumum.blue, set.blue);
  }

  return minumum;
}

function parseLine(line: string): CubeGame {
  const [gamePart, setsPart] = line.split(/\s*:\s*/);
  const game = parseGameId(gamePart);
  const sets = parseSets(setsPart);

  return { game, sets };
}

function parseGameId(gamePart: string): number {
  return parseInt(gamePart.split(/\s+/)[1]);
}

function parseSets(setsPart: string): CubeSet[] {
  return setsPart.split(/\s*;\s*/).map(parseSet);
}

function parseSet(setPart: string): CubeSet {
  const set: CubeSet = { red: 0, green: 0, blue: 0 };

  const items = setPart.split(/\s*,\s*/).forEach((item) => {
    const [countString, color] = item.split(/\s+/);
    const count = parseInt(countString);
    switch (color) {
      case "red":
        set.red = count;
        break;
      case "green":
        set.green = count;
        break;
      case "blue":
        set.blue = count;
        break;
      default:
        throw new Error("parse error");
    }
  });

  return set;
}

export async function solvePart1(input: string): Promise<number> {
  return part1Sync(input.split("\n"));
}

export async function solvePart2(input: string): Promise<number> {
  return part2Sync(input.split("\n"));
}
