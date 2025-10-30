#!/usr/bin/env ts-node
/** Advent of Code - Main verification script */

import * as fs from "fs";

// Import solve functions - these will be implemented in the solution
import { solvePart1, solvePart2 } from "./common";

interface TestResult {
  passed: boolean;
  result: number;
  expected: number;
  duration: number;
}

function runPart(
  name: string,
  solver: (input: string) => number,
  input: string,
  expected: number,
): TestResult {
  const start = performance.now();
  const result = solver(input);
  const duration = performance.now() - start;
  const passed = result === expected;

  return { passed, result, expected, duration };
}

function main() {
  // Read from stdin
  const STDIN_FD = 0;
  const input = fs.readFileSync(STDIN_FD, "utf-8").trim();

  console.log("AoC YYYY Day X: Title");
  console.log("=".repeat(38));

  // Part 1
  const part1 = runPart("Part 1", solvePart1, input, 0); // TODO: Update expected value
  const status1 = part1.passed ? "✅" : "❌";
  console.log(
    `Part 1: ${status1} ${part1.result} (expected: ${part1.expected}) [${part1.duration.toFixed(2)}ms]`,
  );

  // Part 2
  const part2 = runPart("Part 2", solvePart2, input, 0); // TODO: Update expected value
  const status2 = part2.passed ? "✅" : "❌";
  console.log(
    `Part 2: ${status2} ${part2.result} (expected: ${part2.expected}) [${part2.duration.toFixed(2)}ms]`,
  );

  console.log(`Total: ${(part1.duration + part2.duration).toFixed(2)}ms`);

  if (part1.passed && part2.passed) {
    console.log("\n🌟🌟 All tests passed!");
    process.exit(0);
  } else {
    console.log("\n❌ Some tests failed");
    process.exit(1);
  }
}

main();
