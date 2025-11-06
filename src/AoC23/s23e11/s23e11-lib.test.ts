/** AoC 2023 Day 11: Cosmic Expansion - Unit Tests */

import { describe, it, expect } from "vitest";
import { solvePart1, solvePart2 } from "./s23e11-lib";

const SAMPLE = `...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....`;

describe("Day 11: Cosmic Expansion", () => {
  describe("Part 1", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart1(SAMPLE)).toBe(374);
    });
  });

  describe("Part 2", () => {
    it("should solve sample with expansion factor 10 correctly", () => {
      expect(solvePart2(SAMPLE, 10)).toBe(1030);
    });

    it("should solve sample with expansion factor 100 correctly", () => {
      expect(solvePart2(SAMPLE, 100)).toBe(8410);
    });
  });
});
