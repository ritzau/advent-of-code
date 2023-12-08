console.log("Hello, world!!")

const runAllTests = false

async function launch(moduleName: string) {
    const title = moduleName.replace(/.*\//, '')
    console.log(`\n${title}`)
    console.log('='.repeat(title.length))

    const module = await require(moduleName)
    await module.main(runAllTests)
}

async function main() {
    const episodes = Array.from({ length: 25 }, (_, k) =>
        `./AoC23E${(k + 1).toString().padStart(2, '0')}`)

    try {
        for (const episode of episodes) {
            await launch(episode)
        }
    } catch (error) {
        if (error instanceof Error && error.message.startsWith("Cannot find module")) {
            console.log("NYI")
        } else {
            throw error
        }
    }
}

main()
    .then(() => { console.log("\nDone") })
    .catch(e => console.error("\nUnexpected error", e))
