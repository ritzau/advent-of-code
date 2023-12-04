import { createReadStream } from 'node:fs';
import { createInterface } from 'node:readline';
import { Readable } from 'node:stream';

type Card = {
    cardNumber: number
    winningNumbers: number[]
    myNumbers: number[]
}

export async function main() {
    console.log("Part 1 sample:", await part1(createReadStream('AoC23E04-sample.txt')))
    console.log("Part 1 input: ", await part1(createReadStream('AoC23E04-input.txt')))
    console.log("Part 2 sample:", await part2(createReadStream('AoC23E04-sample.txt')))
    console.log("Part 2 input: ", await part2(createReadStream('AoC23E04-input.txt')))
}

async function part1(input: Readable) {
    return asyncSum(totalPointsScored(input))

    async function* totalPointsScored(input: Readable) {
        for await (const line of createInterface(input)) {
            const card = parseLine(line)
            yield pointsScored(card)
        }
    }

    function pointsScored(card: Card) {
        const matchCount = matchingNumbers(card)
        return matchCount === 0 ? 0 : Math.pow(2, matchCount - 1)
    }
}

async function part2(input: Readable) {
    const copies: number[] = []

    for await (const line of createInterface(input)) {
        const card = parseLine(line)
        distributeWin(card)
    }

    return copies.reduce((a, b) => a + b)

    function distributeWin(card: Card) {
        const multiplier = getAndEnsureInitialized(card.cardNumber)
        const matchCount = matchingNumbers(card)

        for (let i = 1; i <= matchCount; ++i) {
            const cardNumber = card.cardNumber + i
            const current = copies[cardNumber] ?? 1
            copies[cardNumber] = current + multiplier
        }
    }

    function getAndEnsureInitialized(index: number) {
        if (copies[index] === undefined) {
            copies[index] = 1
        }
        return copies[index]
    }
}

async function asyncSum(generator: AsyncGenerator<number, void, unknown>) {
    let sum = 0

    for await (const value of generator) {
        sum += value
    }

    return sum
}

function matchingNumbers(card: Card) {
    const mine = new Set(card.myNumbers)
    return card.winningNumbers.filter(x => mine.has(x)).length
}

function parseLine(line: string) {
    const matches = line.match(/^Card\s+(\d*):\s*(.*?)\s*\|\s*(.*?)\s*$/)
    if (matches === null) throw new Error(`Parse error: ${line}`)
    const [, cardNumber, winningNumbers, myNumbers] = matches
    return {
        cardNumber: parseInt(cardNumber),
        winningNumbers: winningNumbers.split(/\s+/).map(x => parseInt(x)),
        myNumbers: myNumbers.split(/\s+/).map(x => parseInt(x))
    }
}
