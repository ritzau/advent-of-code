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
    const episodes = [
        "./AoC23E01",
        "./AoC23E02",
        "./AoC23E03",
        "./AoC23E04",
        "./AoC23E05",
        "./AoC23E06",
    ]

    for (const episode of episodes) {
        await launch(episode)
    }
}

main()
    .then(() => { console.log("\nDone") })
    .catch(e => console.error("\nUnexpected error", e))
