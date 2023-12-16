import * as readline from "node:readline"

import { createReadStream } from "fs"
import { logResult } from "./aoclib"

export async function main() {
    logResult("Part 1 - sample", await part1('AoC23E14-sample.txt'), 136)
    logResult("Part 1 - sample", await part1('AoC23E14-input.txt'), 109833)
}

type Map = string[][]

async function part1(path: string) {
    const readable = createReadStream(path)
    const input = readline.createInterface(readable)
    const map = await parseMap(input)
    slideNorth(map)

    return load(map)
}

async function parseMap(input: readline.Interface) {
    const map: string[][] = []
    for await (const line of input) {
        map.push(Array.from(line))
    }

    return map
}

function slideNorth(map: Map) {
    const stops = new Array(map[0].length).fill(0)

    for (let row = 0; row < map.length; ++row) {
        const line = map[row]
        for (let col = 0; col < line.length; ++col) {
            const element = line[col]
            if (element === 'O') {
                line[col] = '.'
                map[stops[col]][col] = 'O'
                ++stops[col]
            }
            else if (element === '#') {
                stops[col] = row + 1
            }
        }
    }

    return map
}

function load(map: Map) {
    return map.reduce((sum, line, row) => {
        const multiplier = map.length - row
        const count = line.reduce((c, elm) => elm === 'O' ? c + 1 : c, 0)
        return sum + multiplier * count
    }, 0)
}
