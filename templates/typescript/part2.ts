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
const data = fs.readFileSync(0, 'utf-8').trim();
const result = solve(data);
console.log(result);
