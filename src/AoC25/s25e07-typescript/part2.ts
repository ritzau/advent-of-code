#!/usr/bin/env ts-node
/** Advent of Code - Part 2 */

import * as fs from "fs";
import { solvePart2 } from "./common";

// Read from stdin
const STDIN_FD = 0;
const data = fs.readFileSync(STDIN_FD, "utf-8").trim();
const result = solvePart2(data);
console.log(result);
