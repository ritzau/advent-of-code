import fs from 'fs'

type CubeSet = {
    red: number
    green: number
    blue: number
}

type CubeGame = {
    game: number
    sets: CubeSet[]
}

export async function main() {
    console.log("\nAoC23E02")
    console.log("========")

    const sample = fs.readFileSync('AoC23E02-sample.txt').toString().split('\n')
    const input = fs.readFileSync('AoC23E02-input.txt').toString().split('\n')

    console.log("Sample 1:", part1(sample))
    console.log("Part 1:  ", part1(input))
    console.log("Sample 2:", part2(sample))
    console.log("Part 2:  ", part2(input))
}

function part1(input: string[]): number {
    const bag: CubeSet = { red: 12, green: 13, blue: 14 }

    return input
        .filter((l) => l.length > 0)
        .map(parseLine)
        .filter(g => possible(bag, g))
        .map(({ game }) => game)
        .reduce((a, b) => a + b)
}

function possible(bag: CubeSet, { game, sets }: CubeGame): boolean {
    return sets.every(({ red, green, blue }) => red <= bag.red && green <= bag.green && blue <= bag.blue)
}

function part2(input: string[]) {
    return input
        .filter((l) => l.length > 0)
        .map(parseLine)
        .map(minimumSet)
        .map(({ red, green, blue }) => red * green * blue)
        .reduce((a, b) => a + b)
}

function minimumSet({ game, sets }: CubeGame): CubeSet {
    const minumum = sets.pop()
    if (minumum === undefined) {
        throw new Error("No sets in game")
    }

    for (const set of sets) {
        minumum.red = Math.max(minumum.red, set.red)
        minumum.green = Math.max(minumum.green, set.green)
        minumum.blue = Math.max(minumum.blue, set.blue)
    }

    return minumum
}

function parseLine(line: string): CubeGame {
    const [gamePart, setsPart] = line.split(/\s*:\s*/)
    const game = parseGameId(gamePart)
    const sets = parseSets(setsPart)

    return { game, sets }
}

function parseGameId(gamePart: string): number {
    return parseInt(gamePart.split(/\s+/)[1])
}

function parseSets(setsPart: string): CubeSet[] {
    return setsPart.split(/\s*;\s*/).map(parseSet)
}

function parseSet(setPart: string): CubeSet {
    const set: CubeSet = { red: 0, green: 0, blue: 0 }
    
    const items = setPart
        .split(/\s*,\s*/)
        .forEach(item => {
            const [countString, color] = item.split(/\s+/)
            const count = parseInt(countString)
            switch (color) {
                case "red":
                    set.red = count
                    break;
                case "green":
                    set.green = count
                    break;
                case "blue":
                    set.blue = count
                    break;
                default:
                    throw new Error("parse error")
            }
        })

    return set
}