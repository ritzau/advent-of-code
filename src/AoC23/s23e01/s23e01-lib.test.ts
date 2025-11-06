/** AoC 2023 Day 1: Trebuchet?! - Unit Tests */

import { describe, it, expect } from "vitest";
import { solvePart1, solvePart2 } from "./s23e01-lib";

const SAMPLE_1 = `1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet`;

const SAMPLE_2 = `two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen`;

describe("Day 1: Trebuchet?!", () => {
  describe("Part 1", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart1(SAMPLE_1)).toBe(142);
    });
  });

  describe("Part 2", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart2(SAMPLE_2)).toBe(281);
    });
  });
});
