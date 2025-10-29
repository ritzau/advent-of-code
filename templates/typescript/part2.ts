#!/usr/bin/env ts-node
/** Advent of Code - Part 2 */

import * as fs from 'fs';
import { parseInput } from './common';

function solve(data: string): number {
    const lines = parseInput(data);
    // TODO: Implement solution
    return 0;
}

// Read from stdin
// 0 is the file descriptor for stdin
const STDIN_FD = 0;
const data = fs.readFileSync(STDIN_FD, 'utf-8').trim();
const result = solve(data);
console.log(result);
