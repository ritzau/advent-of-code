/** AoC 2023 Day 13: Point of Incidence - Unit Tests */

import { describe, it, expect } from "vitest";
import { solvePart1, solvePart2 } from "./s23e13-lib";

const SAMPLE = `#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#`;

describe("Day 13: Point of Incidence", () => {
  describe("Part 1", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart1(SAMPLE)).toBe(405);
    });
  });

  describe("Part 2", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart2(SAMPLE)).toBe(400);
    });
  });
});
