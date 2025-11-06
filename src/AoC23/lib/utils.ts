/** Shared utilities for Advent of Code 2023 */

import * as fs from "fs";
import * as readline from "node:readline";

// Track test failures globally
let testFailures = 0;

/**
 * Log a test result with formatting
 */
export function logResult(message: string, actual: number, expected: number) {
  const passed = actual === expected;
  if (!passed) {
    testFailures++;
  }
  console.log(
    `${message} `.padEnd(24, "_") + ` ${actual}`.padStart(16, "_"),
    passed ? "✅" : "❌",
  );
}

/**
 * Exit with appropriate status code based on test results
 */
export function exitWithTestStatus() {
  if (testFailures > 0) {
    console.log(`\n❌ ${testFailures} test(s) failed`);
    process.exit(1);
  } else {
    console.log("\n✅ All tests passed");
    process.exit(0);
  }
}

/**
 * Sum values from an async generator
 */
export async function asyncSum(
  generator: AsyncGenerator<number, void, unknown>,
) {
  let sum = 0;

  for await (const value of generator) {
    sum += value;
  }

  return sum;
}

/**
 * Result from reading a line
 */
export type ReadLineResult = {
  done: boolean;
  line: string;
};

/**
 * Read a single line from readline interface
 */
export async function readLine(
  input: readline.Interface,
): Promise<ReadLineResult> {
  for await (const line of input) {
    return { done: false, line };
  }
  return { done: true, line: "" };
}

/**
 * Read a line or throw if EOF
 */
export async function expectLine(input: readline.Interface) {
  for await (const line of input) {
    return line;
  }

  throw new Error("Unexpected EOF");
}

/**
 * Expect a blank line or throw
 */
export async function expectBlankLine(input: readline.Interface) {
  for await (const line of input) {
    if (line.length !== 0) {
      throw new Error(`Unexpected non-blank line: ${line}`);
    }
    return;
  }

  throw new Error("Unexpected EOF");
}

/**
 * Sum an array of numbers
 */
export function sum(values: number[]) {
  return values.reduce((a, b) => a + b, 0);
}

/**
 * Run an AoC solution by reading from stdin and printing to stdout
 */
export function runSolution(solveFn: (input: string) => number | string): void {
  const STDIN_FD = 0;
  const data = fs.readFileSync(STDIN_FD, "utf-8").trim();
  const result = solveFn(data);
  console.log(result);
}
