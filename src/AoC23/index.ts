console.log("Hello, world!!")

async function launch(module: string) {
    (await require(module)).main()
}

try {
    const episodes = [
        "./AoC23E01",
        "./AoC23E02",
    ]

    for (const episode of episodes) {
        launch(episode)
    }
} catch (exception) {
    console.error(exception)
}