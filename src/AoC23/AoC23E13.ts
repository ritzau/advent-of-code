import * as readline from "node:readline"

import { createReadStream } from "fs"
import { logResult } from "./aoclib"
import assert from "node:assert"

export async function main() {
    logResult("Part 1 - sample", await part1('AoC23E13-sample.txt'), 405)
    logResult("Part 1 - input", await part1('AoC23E13-input.txt'), 31877)
    logResult("Part 2 - sample", await part2('AoC23E13-sample.txt'), 400)
    logResult("Part 2 - input", await part2('AoC23E13-input.txt'), 42996)
}

async function part1(path: string) {
    const readable = createReadStream(path)
    const input = readline.createInterface(readable)

    let sum = 0
    for await (const map of parseMap(input)) {
        const reflectionLine = findReflection(map)
        const reflectionColumn = findReflection(columns(map))

        assert(reflectionLine === -1 || reflectionColumn === -1)
        assert(reflectionLine !== -1 || reflectionColumn !== -1)

        if (reflectionLine !== -1) {
            sum += 100 * reflectionLine
        }

        if (reflectionColumn !== -1) {
            sum += reflectionColumn
        }
    }

    return sum
}

async function part2(path: string) {
    const readable = createReadStream(path)
    const input = readline.createInterface(readable)

    let sum = 0
    for await (const map of parseMap(input)) {
        const reflectionLine = findSmudgedReflection(map)
        const reflectionColumn = findSmudgedReflection(columns(map))

        assert(reflectionLine === -1 || reflectionColumn === -1)
        assert(reflectionLine !== -1 || reflectionColumn !== -1)

        if (reflectionLine !== -1) {
            sum += 100 * reflectionLine
        }

        if (reflectionColumn !== -1) {
            sum += reflectionColumn
        }
    }

    return sum
}

async function* parseMap(input: readline.Interface) {
    const lines: string[] = []

    for await (const line of input) {
        if (line.length === 0) {
            yield lines
            lines.length = 0
        } else {
            lines.push(line)
        }
    }

    if (lines.length > 0) {
        yield lines
    }
}

function columns(map: string[]) {
    const cols: string[] = []
    for (let i = 0; i < map[0].length; ++i) {
        cols.push(Array.from(map, l => l[i]).join(''))
    }

    return cols
}

function findReflection(list: string[]) {
    const reflections: number[] = []

    let previous = list[0]
    for (let i = 1; i < list.length; ++i) {
        const current = list[i]
        if (current === previous) {
            if (testReflection(list, i)) {
                reflections.push(i)
            }
        }
        previous = current
    }

    assert(reflections.length <= 1)

    return reflections.length === 0 ? -1 : reflections[0]
}

type SimilarPair = { indexA: number, indexB: number, distance: number }

function findSmudgedReflection(map: string[]) {
    const pairs: SimilarPair[] = []

    for (const [indexA, itemA] of map.entries()) {
        const ids = map.slice(indexA + 1)
            .map((itemB, indexB) => [indexA + indexB + 1, itemB] as [number, string])
            .map(([indexB, itemB]) => ({
                indexA,
                indexB,
                distance: stringDistance(itemA, itemB)
            }))
            .filter(p => p.distance <= 1)

        pairs.push(...ids)
    }

    const reflection = pairs
        .filter(x => x.indexB - x.indexA === 1)
        .filter(candidate => testSmudgedReflection(map.length, pairs, candidate))
        .map(c => c.indexB)

    assert(reflection.length <= 1)

    return reflection.length === 0 ? -1 : reflection[0]
}

function stringDistance(a: string, b: string) {
    assert(a.length === b.length)

    let mismatches = 0
    for (let i = 0; i < a.length && mismatches <= 1; ++i) {
        if (a[i] !== b[i]) {
            ++mismatches
        }
    }

    return mismatches
}

function testSmudgedReflection(n: number, items: SimilarPair[], candidate: SimilarPair) {
    let sum = 0
    let delta = 0
    const index = candidate.indexB

    while (0 <= (index - 1) - delta && index + delta < n) {
        const pair = items.find(p => p.indexA === index - 1 - delta && p.indexB === index + delta)
        if (pair === undefined) {
            return false
        }

        sum += pair.distance
        if (sum > 1) return false

        ++delta
    }

    return sum === 1
}

function testReflection(list: string[], index: number) {
    let delta = 0

    while (0 <= (index - 1) - delta && index + delta < list.length) {
        if (list[index - 1 - delta] !== list[index + delta]) {
            return false
        }
        ++delta
    }

    return true
}

function printMapH(map: string[], split: number) {
    for (let i = 0; i < map.length; ++i) {
        if (i === split) {
            console.log(' '.repeat(4), '-'.repeat(map[i].length))
        }
        console.log((i + 1).toString().padStart(4), map[i])
    }
}

function printMapV(map: string[], split: number) {
    const head = Array.from({ length: map[0].length }, (_, i) => ((i + 1) % 10).toString()).join('')
    console.log(' '.repeat(4), head.substring(0, split) + '|' + head.substring(split))

    for (let i = 0; i < map.length; ++i) {
        console.log((i + 1).toString().padStart(4), map[i].substring(0, split) + '|' + map[i].substring(split))
    }
}
