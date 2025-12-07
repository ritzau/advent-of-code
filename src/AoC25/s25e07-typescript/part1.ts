#!/usr/bin/env ts-node
/** Advent of Code - Part 1 */

import * as fs from "fs";
import { solvePart1 } from "./common";

// Read from stdin
const STDIN_FD = 0;
const data = fs.readFileSync(STDIN_FD, "utf-8").trim();
const result = solvePart1(data);
console.log(result);
