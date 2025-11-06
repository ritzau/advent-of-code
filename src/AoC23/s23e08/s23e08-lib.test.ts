/** AoC 2023 Day 8: Haunted Wasteland - Unit Tests */

import { describe, it, expect } from "vitest";
import { solvePart1, solvePart2 } from "./s23e08-lib";

const SAMPLE_1 = `RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)`;

const SAMPLE_2 = `LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)`;

const SAMPLE_3 = `LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)`;

describe("Day 8: Haunted Wasteland", () => {
  describe("Part 1", () => {
    it("should solve sample 1 correctly", () => {
      expect(solvePart1(SAMPLE_1)).toBe(2);
    });

    it("should solve sample 2 correctly", () => {
      expect(solvePart1(SAMPLE_2)).toBe(6);
    });
  });

  describe("Part 2", () => {
    it("should solve sample 3 correctly", () => {
      expect(solvePart2(SAMPLE_3)).toBe(6);
    });
  });
});
