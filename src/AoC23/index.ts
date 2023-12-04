console.log("Hello, world!!")

async function launch(moduleName: string) {
    const title = moduleName.replace(/.*\//, '')
    console.log(`\n${title}`)
    console.log('='.repeat(title.length))

    const module = await require(moduleName)
    await module.main()
}

async function main() {
    const episodes = [
        "./AoC23E01",
        "./AoC23E02",
        "./AoC23E03",
        "./AoC23E04",
        "./AoC23E05",
    ]

    for (const episode of episodes) {
        await launch(episode)
    }
}

main()
    .then(() => { console.log("\nDone")})
    .catch(e => console.error("\nUnexpected error", e))
