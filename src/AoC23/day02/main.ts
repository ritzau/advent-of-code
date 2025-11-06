import fs from "fs";
import { logResult } from "../lib/utils";
import { solvePart1, solvePart2 } from "./common";

async function main() {
  const sample = fs.readFileSync("day02/sample.txt").toString();
  const input = fs.readFileSync("../inputs/day02.txt").toString();

  logResult("Part 1 - sample", await solvePart1(sample), 8);
  logResult("Part 1 - input", await solvePart1(input), 2632);
  logResult("Part 2 - sample", await solvePart2(sample), 2286);
  logResult("Part 2 - input", await solvePart2(input), 69629);
}

main();
