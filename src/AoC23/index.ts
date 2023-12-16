import assert from "node:assert"

console.log("Hello, world!!")

const runAllTests = false

async function launch(moduleName: string) {
    console.log()
    const title = moduleName.replace(/.*\//, '')
    console.group(title)
    console.time(title)
    try {
        const module = await require(moduleName)
        await module.main(runAllTests)
        return true
    } catch (error) {
        if (error instanceof Error && error.message.startsWith("Cannot find module")) {
            console.log("NYI")
            return false
        } else {
            throw error
        }
    } finally {
        console.timeEnd(title)
        console.groupEnd()
    }
}

async function main() {
    for (const episode of episodes()) {
        if (!await launch(episode)) break
    }
}

function episodes(start: number | undefined = undefined, end: number | undefined = undefined) {
    if (start === undefined && end === undefined) {
        return episodeRange(1, 25)
    }

    if (start !== undefined && end !== undefined) {
        return episodeRange(start, end)
    }

    if (start !== undefined && end === undefined) {
        return episodeRange(start, start)
    }

    if (start === undefined && end !== undefined) {
        return episodeRange(1, end)
    }

    assert.fail()
}

function episodeRange(start: number, end: number) {
    return Array.from({ length: end - start + 1 }, (_, k) =>
        `./AoC23E${(k + start).toString().padStart(2, '0')}`)
}

main()
    .then(() => { console.log("\nDone") })
    .catch(e => console.error("\nUnexpected error", e))
