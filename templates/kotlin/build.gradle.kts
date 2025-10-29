plugins {
    kotlin("jvm") version "2.0.21"
    application
}

group = "com.adventofcode"
version = "1.0.0"

repositories {
    mavenCentral()
}

dependencies {
    implementation(kotlin("stdlib"))
    testImplementation(kotlin("test"))
    testImplementation("org.junit.jupiter:junit-jupiter:5.10.1")
}

tasks.test {
    useJUnitPlatform()
}

kotlin {
    jvmToolchain(17)
}

// Main verification binary
application {
    mainClass.set("MainKt")
}

// Create JAR task for the main verification binary
tasks.register<Jar>("mainJar") {
    archiveBaseName.set("aoc-solution")
    manifest {
        attributes["Main-Class"] = "MainKt"
    }
    from(sourceSets.main.get().output)
    dependsOn(configurations.runtimeClasspath)
    from({
        configurations.runtimeClasspath.get().filter { it.name.endsWith("jar") }.map { zipTree(it) }
    })
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
}

// Create JAR task for Part 1
tasks.register<Jar>("part1Jar") {
    archiveBaseName.set("part1")
    manifest {
        attributes["Main-Class"] = "Part1Kt"
    }
    from(sourceSets.main.get().output)
    dependsOn(configurations.runtimeClasspath)
    from({
        configurations.runtimeClasspath.get().filter { it.name.endsWith("jar") }.map { zipTree(it) }
    })
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
}

// Create JAR task for Part 2
tasks.register<Jar>("part2Jar") {
    archiveBaseName.set("part2")
    manifest {
        attributes["Main-Class"] = "Part2Kt"
    }
    from(sourceSets.main.get().output)
    dependsOn(configurations.runtimeClasspath)
    from({
        configurations.runtimeClasspath.get().filter { it.name.endsWith("jar") }.map { zipTree(it) }
    })
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
}

// Convenience task to build all JARs
tasks.register("buildAll") {
    dependsOn("mainJar", "part1Jar", "part2Jar")
}
