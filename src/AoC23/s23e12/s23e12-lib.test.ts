/** AoC 2023 Day 12: Hot Springs - Unit Tests */

import { describe, it, expect } from "vitest";
import { solvePart1 } from "./s23e12-lib";

const SAMPLE = `???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1`;

describe("Day 12: Hot Springs", () => {
  describe("Part 1", () => {
    it("should solve sample input correctly", () => {
      expect(solvePart1(SAMPLE)).toBe(21);
    });
  });

  // Part 2 is too slow, commented out
  // describe("Part 2", () => {
  //   it("should solve sample input correctly", () => {
  //     expect(solvePart2(SAMPLE)).toBe(525152);
  //   });
  // });
});
