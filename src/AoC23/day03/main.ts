import fs from "fs";
import { logResult } from "../lib/utils";
import { solvePart1, solvePart2 } from "./common";

async function main() {
  const sample = fs.readFileSync("day03/sample.txt").toString();
  const input = fs.readFileSync("../inputs/day03.txt").toString();

  logResult("Part 1 - sample", await solvePart1(sample), 4361);
  logResult("Part 1 - input", await solvePart1(input), 544664);
  logResult("Part 2 - sample", await solvePart2(sample), 467835);
  logResult("Part 2 - input", await solvePart2(input), 84495585);
}

main();
