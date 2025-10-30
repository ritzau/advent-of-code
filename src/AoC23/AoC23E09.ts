import * as readline from "node:readline";

import { createReadStream } from "fs";
import { logResult, sum } from "./aoclib";
import assert from "node:assert";

export async function main() {
  logResult("Part 1 - sample", await part1("AoC23E09-sample.txt"), 114);
  logResult("Part 1 - input", await part1("AoC23E09-input.txt"), 2098530125);
  logResult("Part 2 - sample", await part2("AoC23E09-sample.txt"), 2);
  logResult("Part 2 - input", await part2("AoC23E09-input.txt"), 1016);
}

async function part1(path: string) {
  return processHistories(path, extrapolate);
}

async function part2(path: string) {
  return processHistories(path, extrapolateBack);
}

async function processHistories(
  path: string,
  callback: (history: number[]) => number,
) {
  const reader = createReadStream(path);
  const input = readline.createInterface(reader);
  const histories = await parseHistories(input);
  return sum(histories.map((h) => callback(h)));
}

async function parseHistories(input: readline.Interface) {
  const histories: number[][] = [];
  for await (const line of input) {
    const history = line.split(/\s+/).map((w) => parseInt(w));
    histories.push(history);
  }

  return histories;
}

function extrapolate(history: number[]) {
  return findDiff(history)
    .map((vs) => vs[vs.length - 1])
    .reduce((a, b) => a + b);
}

function extrapolateBack(history: number[]) {
  return findDiff(history)
    .map((vs) => vs[0])
    .reverse()
    .reduce((a, b) => b - a);
}

function findDiff(history: number[]) {
  const stack = [history];

  while (!stack[stack.length - 1].every((x, _, a) => x == a[0])) {
    assert(stack[stack.length - 1].length > 1);
    const vs = stack[stack.length - 1];
    const ds = vs.slice(1).map((v, i) => v - vs[i]);
    stack.push(ds);
  }

  return stack;
}
