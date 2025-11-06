/** AoC 2023 Day 2: Cube Conundrum - Unit Tests */

import { describe, it, expect } from "vitest";
import { solvePart1, solvePart2 } from "./s23e02-lib";

const SAMPLE = `Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green`;

describe("Day 2: Cube Conundrum", () => {
  describe("Part 1", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart1(SAMPLE)).toBe(8);
    });
  });

  describe("Part 2", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart2(SAMPLE)).toBe(2286);
    });
  });
});
