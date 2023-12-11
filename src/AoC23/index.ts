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
    const episodes = Array.from({ length: 25 }, (_, k) =>
        `./AoC23E${(k + 1).toString().padStart(2, '0')}`)

    for (const episode of episodes) {
        if (!await launch(episode)) break
    }
}

main()
    .then(() => { console.log("\nDone") })
    .catch(e => console.error("\nUnexpected error", e))
