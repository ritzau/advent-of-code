import { createReadStream } from "node:fs"
import { Readable } from "node:stream"

export async function main() {
    console.log("Part 1 sample:", await part1(createReadStream('AoC23E05-sample.txt')))
    console.log("Part 1 input: ", await part1(createReadStream('AoC23E05-input.txt')))
    console.log("Part 2 sample:", await part2(createReadStream('AoC23E05-sample.txt')))
    console.log("Part 2 input: ", await part2(createReadStream('AoC23E05-input.txt')))
}

async function part1(reader: Readable) {
    return 42
}

async function part2(reader: Readable) {
    return 1337
}
