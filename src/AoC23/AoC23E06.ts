import assert from "node:assert"
import { createReadStream } from "node:fs"
import * as readline from "node:readline"
import { logResult } from "./aoclib"

export async function main() {
    logResult("Part 1 - sample", await part1('AoC23E06-sample.txt'), 288)
    logResult("Part 1 - input", await part1('AoC23E06-input.txt'), 1413720)
    logResult("Part 2 - sample", await part2('AoC23E06-sample.txt'), 71503)
    logResult("Part 2 - input", await part2('AoC23E06-input.txt'), 30565288)
}

type RaceParameters = {
    time: number
    recordDistance: number
}

async function part1(path: string) {
    const readable = createReadStream(path)
    const input = readline.createInterface(readable)
    const raceParameters = await parseRaceParameters(input)

    return solveRaces(raceParameters)
        .reduce((a, b) => a * b)
}

async function part2(path: string) {
    const readable = createReadStream(path)
    const input = readline.createInterface(readable)
    const raceParameters = [await parseRaceParameters2(input)]

    return solveRaces(raceParameters)[0]
}

function solveRaces(raceParameters: RaceParameters[]) {
    return raceParameters.map(({ time, recordDistance }) => {
        const roots = solveQuadratic(-1, time, -recordDistance)
        assert.strictEqual(roots.length, 2)
        return lowerBoundary(roots[1]) - upperBoundary(roots[0]) + 1
    })
}

async function parseRaceParameters(input: readline.Interface) {
    const lines: number[][] = []
    for await (const line of input) {
        const values = line.split(/\s+/).slice(1).map(x => parseInt(x))
        lines.push(values)
    }

    assert.strictEqual(lines.length, 2)
    assert.strictEqual(lines[0].length, lines[1].length)

    return Array.from({ length: lines[0].length }, (_, i) => ({
        time: lines[0][i],
        recordDistance: lines[1][i],
    }));
}

async function parseRaceParameters2(input: readline.Interface) {
    const lines: number[] = []
    for await (const line of input) {
        const value = parseInt(line.split(/\s+/).slice(1).join(''))
        lines.push(value)
    }

    assert.strictEqual(lines.length, 2)

    return {
        time: lines[0],
        recordDistance: lines[1],
    }
}

function solveQuadratic(a: number, b: number, c: number): number[] {
    const discriminant = b * b - 4 * a * c;
    if (discriminant < 0) {
        return [];
    } else if (discriminant === 0) {
        return [-b / (2 * a)];
    } else {
        const root1 = (-b + Math.sqrt(discriminant)) / (2 * a);
        const root2 = (-b - Math.sqrt(discriminant)) / (2 * a);
        return [root1, root2];
    }
}

function upperBoundary(x: number) {
    if (Number.isInteger(x)) {
        return x === 0 ? x : x + 1
    }
    return Math.ceil(x)
}

function lowerBoundary(x: number) {
    if (Number.isInteger(x)) {
        return x === 0 ? x : x - 1
    }
    return Math.floor(x)
}
