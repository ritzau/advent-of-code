#!/usr/bin/env ts-node
/** Advent of Code - Part 1 */

import * as fs from 'fs';
import { parseInput } from './common';

function solve(data: string): number {
    const lines = parseInput(data);
    // TODO: Implement solution
    return 0;
}

// Read from stdin
const STDIN_FD = 0; // File descriptor for stdin
const data = fs.readFileSync(STDIN_FD, 'utf-8').trim();
const result = solve(data);
console.log(result);
