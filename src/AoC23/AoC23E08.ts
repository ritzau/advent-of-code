import * as readline from "node:readline"

import { createReadStream } from "fs"
import { expectBlankLine, expectLine, logResult } from "./aoclib"

export async function main() {
    logResult("Part 1 - sample 1", await part1('AoC23E08-sample-1.txt'), 2)
    logResult("Part 1 - sample 2", await part1('AoC23E08-sample-2.txt'), 6)
    logResult("Part 1 - input", await part1('AoC23E08-input.txt'), 19241)
    logResult("Part 2 - sample 3", await part2('AoC23E08-sample-3.txt'), 6)
    logResult("Part 2 - input", await part2('AoC23E08-input.txt'), 9606140307013)
}

async function part1(file: string) {
    const map = await parseMapFromFile(file)
    return walkMap(map.direction, map.nodes, 'AAA', n => n === 'ZZZ')
}

async function part2(file: string) {
    const map = await parseMapFromFile(file)

    const steps = Array.from(map.nodes.keys())
        .filter(n => n.endsWith('A'))
        .map(s => walkMap(map.direction, map.nodes, s, n => n.endsWith('Z')))

    return lcmOfArray(steps)
}

enum Direction {
    LEFT,
    RIGHT,
}

type Node = {
    label: string,
    left: string,
    right: string,
}

async function parseMapFromFile(file: string) {
    const reader = createReadStream(file)
    const input = readline.createInterface(reader)
    return await parseMap(input)
}

async function parseMap(input: readline.Interface) {
    const direction = await parseDirection(input)
    const nodes = await parseNodes(input)
    return { direction, nodes }
}

async function parseDirection(input: readline.Interface) {
    const directionMap: { [key: string]: Direction } = {
        'L': Direction.LEFT,
        'R': Direction.RIGHT,
    }

    const line = await expectLine(input)
    await expectBlankLine(input)

    return Array.from(line).map(c => directionMap[c])
}

async function parseNodes(input: readline.Interface) {
    const nodes = new Map<string, [string, string]>()
    for await (const line of input) {
        const m = line.match(/^([0-9A-Z]{3}) = \(([0-9A-Z]{3}), ([0-9A-Z]{3})\)$/)
        if (m === null) throw new Error("Parse error")
        const [, label, left, right] = m
        nodes.set(label, [left, right])
    }

    return nodes
}

function walkMap(
    direction: Direction[],
    nodes: Map<string, [string, string]>,
    start: string,
    predicate: (node: string) => boolean,
) {
    let steps = 0
    let currentNode = start
    while (!predicate(currentNode)) {
        const step = direction[steps % direction.length]
        const node = nodes.get(currentNode) as [string, string]
        currentNode = node[step === Direction.LEFT ? 0 : 1]
        ++steps
    }

    return steps
}

function gcd(a: number, b: number): number {
    while (b !== 0) {
        let temp = b;
        b = a % b;
        a = temp;
    }

    return a;
}

function lcm(a: number, b: number): number {
    return Math.abs(a * b) / gcd(a, b);
}

function lcmOfArray(arr: number[]): number {
    let currentLcm = arr[0];
    for (let i = 1; i < arr.length; i++) {
        currentLcm = lcm(currentLcm, arr[i]);
    }

    return currentLcm;
}
