import { describe, test } from "node:test";
import assert from "node:assert";
import { solvePart1, solvePart2 } from "./common.js";

const sampleInput = `R2, L3`;

describe("Part 1", () => {
  test("should solve sample input correctly", () => {
    const result = solvePart1(sampleInput);
    assert.strictEqual(result, 5);
  });
});

describe("Part 2", () => {
  test("should solve sample input correctly", () => {
    const result = solvePart2("R8, R4, R4, R8");
    assert.strictEqual(result, 4);
  });
});
