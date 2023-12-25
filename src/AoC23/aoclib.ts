import { existsSync, readFileSync } from "node:fs";
import * as readline from "node:readline";
import { createWriteStream, promises as fs } from 'fs';
import { mkdir, writeFile } from "node:fs/promises";

export function logResult(message: string, actual: number, expected: number) {
    console.log(`${message} `.padEnd(24, '_') + ` ${actual}`.padStart(16, '_'), actual === expected ? "✅" : "❌");
}

export async function asyncSum(generator: AsyncGenerator<number, void, unknown>) {
    let sum = 0;

    for await (const value of generator) {
        sum += value;
    }

    return sum;
}

export type ReadLineResult = {
    done: boolean;
    line: string;
};

export async function readLine(input: readline.Interface): Promise<ReadLineResult> {
    for await (const line of input) {
        return { done: false, line };
    }
    return { done: true, line: '' };
}

export async function expectLine(input: readline.Interface) {
    for await (const line of input) {
        return line;
    }

    throw new Error("Unexpected EOF");
}

export async function expectBlankLine(input: readline.Interface) {
    for await (const line of input) {
        if (line.length !== 0) {
            throw new Error(`Unexpected non-bank line: ${line}`);
        }
        return;
    }

    throw new Error("Unexpected EOF");
}

export function sum(values: number[]) {
    return values.reduce((a, b) => a + b, 0);
}

export async function getAocInputFile(day: number, year = 2023, force = false) {
    const filename = `aoc-cache/s${year}e${day}-input.txt`;

    if (existsSync(filename)) return filename;

    await mkdir('aoc-cache', { recursive: true });

    const sessionCookie = (await fs.readFile("aoc-session-cookie")).toString().trim();
    console.debug("Sending request to get input for year:", year, "day:", day);

    const response = await fetch(`https://adventofcode.com/${year}/day/${day}/input`, {
        headers: new Headers({
            "Cookie": `session=${sessionCookie}`,
        }),
    });

    if (!response.ok) {
        throw new Error(`Failed request: ${response.statusText}, ${response.status}`);
    }

    console.debug("Streaming input to:", filename);
    await writeFile(filename, await response.text());

    return filename;
}

export async function readAocInputFile(date: number, year = 2023) {
    const path = await getAocInputFile(date, year);
    return readFileSync(path).toString();
}
