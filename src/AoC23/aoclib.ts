export function logResult(message: string, actual: number, expected: number) {
    console.log(message.padEnd(24, '_') + actual.toString().padStart(12, '_'), actual === expected ? "✅" : "❌")
}

export async function asyncSum(generator: AsyncGenerator<number, void, unknown>) {
    let sum = 0

    for await (const value of generator) {
        sum += value
    }

    return sum
}
