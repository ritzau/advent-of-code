import fs from "fs";
import { logResult } from "../lib/utils";
import { solvePart1, solvePart2 } from "./common";

async function main() {
  const sample = fs.readFileSync("day04/sample.txt").toString();
  const input = fs.readFileSync("../inputs/day04.txt").toString();

  logResult("Part 1 - sample", await solvePart1(sample), 13);
  logResult("Part 1 - input", await solvePart1(input), 22674);
  logResult("Part 2 - sample", await solvePart2(sample), 30);
  logResult("Part 2 - input", await solvePart2(input), 5747443);
}

main();
