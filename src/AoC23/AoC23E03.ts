import { open } from 'node:fs/promises';
import { Interface } from 'readline';

export async function main() {
    console.log("Part 1 sample:", await part1(readLinesFromFile('AoC23E03-sample.txt')))
    console.log("Part 1 input: ", await part1(readLinesFromFile('AoC23E03-input.txt')))
    console.log("Part 2 sample:", await part2(readLinesFromFile('AoC23E03-sample.txt')))
    console.log("Part 2 input: ", await part2(readLinesFromFile('AoC23E03-input.txt')))
}

async function readLinesFromFile(path: string): Promise<Interface> {
    const file = await open(path)
    return file.readLines()
}

async function asyncSum(generator: AsyncGenerator<number, void, unknown>): Promise<number> {
    let sum = 0
    for await (const p of generator) {
        sum += p
    }

    return sum
}

async function part1(lineReader: Promise<Interface>): Promise<number> {
    return await asyncSum(partNumbers(await lineReader))
}

async function part2(lineReader: Promise<Interface>): Promise<number> {
    return await asyncSum(gearRatios(await lineReader))
}


async function* partNumbers(asyncLines: Interface) {
    function* handleNeighours(ns: Number[]) {
        yield* ns.map(n => parseInt(n.value))
    }

    const schematics = new EngineSchematic(c => isSymbol(c), handleNeighours)

    for await (const line of asyncLines) {
        if (line.length === 0) continue;
        yield* schematics.addLine(line)
    }
    yield* schematics.addLastLine()
}

type Number = {
    startIndex: number
    lastIndex: number
    value: string
    id: number
}

async function* gearRatios(asyncLines: Interface) {
    function* handleNeighours(ns: Number[]) {
        if (ns.length === 2) {
            yield ns.map(n => parseInt(n.value)).reduce((a, b) => a * b);
        }
    }

    const schematics = new EngineSchematic(x => x === '*', handleNeighours)

    for await (const line of asyncLines) {
        if (line.length === 0) continue;
        yield* schematics.addLine(line)
    }
    yield* schematics.addLastLine()
}

function isSymbol(x: string): boolean {
    return x.match(/[^\d.]/) !== null
}


function isDigit(x: string) {
    return x.match(/\d/)
}

type SymbolPredicate = (x: string) => boolean

type NeighbourHandler = {
    (ns: Number[]): Generator<number, void, unknown>; (ns: Number[]): Generator<number, void, unknown>
}

class EngineSchematic {
    private lines: string[][] = []
    private numbers: Number[][] = []
    private nextId = 0

    private isSymbol: SymbolPredicate
    private handleNeighbours: NeighbourHandler

    constructor(isSymbol: SymbolPredicate, handleNeighbours: NeighbourHandler) {
        this.isSymbol = isSymbol
        this.handleNeighbours = handleNeighbours
    }

    async * addLine(line: string) {
        while (this.lines.length > 2) {
            this.lines.shift()
            this.numbers.shift()
        }

        if (this.lines.length === 0) {
            this.lines.push(Array.from('.'.repeat(line.length)))
            this.numbers.push([])
        }

        const lineArray = Array.from(line)
        this.lines.push(lineArray)
        this.numbers.push(this.parseNumbers(lineArray))

        if (this.lines.length >= 3) {
            const symbolIndices = Array.from(this.lines[1]).flatMap((c, i) => this.isSymbol(c) ? i : [])
            for (const g of symbolIndices) {
                yield* this.findNeighbours(g);
            }
        }
    }

    async * addLastLine() {
        yield* this.addLine('.'.repeat(this.lines[0].length))
    }

    private *findNeighbours(g: number) {
        const neighbourNumbers = new Map<number, Number>();
        for (let r = 0; r <= 2; ++r) {
            for (let c = -1; c <= 1; ++c) {
                if (r === 1 && c === 0) continue;
                const n = this.findNumber(r, g + c);
                if (n !== undefined) neighbourNumbers.set(n.id, n);
            }
        }

        yield* this.handleNeighbours(Array.from(neighbourNumbers.values()))
    }

    private findNumber(row: number, col: number) {
        return this.numbers[row].find(n => n.startIndex <= col && col <= n.lastIndex)
    }

    private parseNumbers(line: string[]): Number[] {
        let isNumber = false
        const ns: Number[] = []
        let n: Number = {
            startIndex: -1,
            lastIndex: -1,
            value: '',
            id: -1,
        }

        for (const [i, c] of line.entries()) {
            if (isNumber) {
                if (isDigit(c)) {
                    n.value += c
                    n.lastIndex = i
                } else {
                    isNumber = false
                    n = {
                        startIndex: -1,
                        lastIndex: -1,
                        value: '',
                        id: -1,
                    }
                }
            } else {
                if (isDigit(c)) {
                    ns.push(n)
                    n.value += c
                    n.startIndex = i
                    n.lastIndex = i
                    n.id = this.nextId++
                    isNumber = true
                }
            }
        }

        return ns
    }
}
