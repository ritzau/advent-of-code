/** AoC 2023 Day 9: Mirage Maintenance - Unit Tests */

import { describe, it, expect } from "vitest";
import { solvePart1, solvePart2 } from "./s23e09-lib";

const SAMPLE = `0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45`;

describe("Day 9: Mirage Maintenance", () => {
  describe("Part 1", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart1(SAMPLE)).toBe(114);
    });
  });

  describe("Part 2", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart2(SAMPLE)).toBe(2);
    });
  });
});
