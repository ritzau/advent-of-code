/** AoC 2023 Day 7: Camel Cards - Unit Tests */

import { describe, it, expect } from "vitest";
import { solvePart1, solvePart2 } from "./s23e07-lib";

const SAMPLE = `32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483`;

describe("Day 7: Camel Cards", () => {
  describe("Part 1", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart1(SAMPLE)).toBe(6440);
    });
  });

  describe("Part 2", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart2(SAMPLE)).toBe(5905);
    });
  });
});
