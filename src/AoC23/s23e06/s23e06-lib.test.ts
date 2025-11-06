/** AoC 2023 Day 6: Wait For It - Unit Tests */

import { describe, it, expect } from "vitest";
import { solvePart1, solvePart2 } from "./s23e06-lib";

const SAMPLE = `Time:      7  15   30
Distance:  9  40  200`;

describe("Day 6: Wait For It", () => {
  describe("Part 1", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart1(SAMPLE)).toBe(288);
    });
  });

  describe("Part 2", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart2(SAMPLE)).toBe(71503);
    });
  });
});
