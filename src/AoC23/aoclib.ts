import * as readline from "node:readline"

export function logResult(message: string, actual: number, expected: number) {
    console.log(`${message} `.padEnd(24, '_') + ` ${actual}`.padStart(16, '_'), actual === expected ? "✅" : "❌")
}

export async function asyncSum(generator: AsyncGenerator<number, void, unknown>) {
    let sum = 0

    for await (const value of generator) {
        sum += value
    }

    return sum
}

export type ReadLineResult = {
    done: boolean
    line: string
}

export async function readLine(input: readline.Interface): Promise<ReadLineResult> {
    for await (const line of input) {
        return { done: false, line }
    }
    return { done: true, line: '' }
}

export async function expectLine(input: readline.Interface) {
    for await (const line of input) {
        return line
    }

    throw new Error("Unexpected EOF")
}

export async function expectBlankLine(input: readline.Interface) {
    for await (const line of input) {
        if (line.length !== 0) {
            throw new Error(`Unexpected non-bank line: ${line}`)
        }
        return
    }

    throw new Error("Unexpected EOF")
}

export function sum(values: number[]) {
    return values.reduce((a, b) => a + b, 0)
}
