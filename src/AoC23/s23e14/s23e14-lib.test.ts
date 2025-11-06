/** AoC 2023 Day 14: Parabolic Reflector Dish - Unit Tests */

import { describe, it, expect } from "vitest";
import { solvePart1 } from "./s23e14-lib";

const SAMPLE = `O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....`;

describe("Day 14: Parabolic Reflector Dish", () => {
  describe("Part 1", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart1(SAMPLE)).toBe(136);
    });
  });
});
