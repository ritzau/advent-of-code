import org.gradle.jvm.application.tasks.CreateStartScripts
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

allprojects {
    repositories {
        System.getenv()["NIX_MAVEN_REPO"]?.let {
            mavenLocal {
                url = uri(it)
                metadataSources {
                    mavenPom()
                    gradleMetadata()
                }
            }
        }
            ?: run { mavenCentral() }
    }
}

plugins {
    kotlin("jvm") version "1.9.23"
    application
}

group = "com.ritzau.aoc"

version = "1.0-SNAPSHOT"

java {
    sourceCompatibility = JavaVersion.VERSION_21
    targetCompatibility = JavaVersion.VERSION_21
}

dependencies {
    testImplementation(kotlin("test"))
    testImplementation("org.junit.jupiter:junit-jupiter-api:5.8.2")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:5.8.2")
}

tasks.test {
    useJUnitPlatform()
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "21"
}

application {
    mainClass.set("MainKt")
    applicationName = "s16e01-kotlin"
}

// Create start script for Part 1
val createPart1StartScripts =
    tasks.register<CreateStartScripts>("createPart1StartScripts") {
        applicationName = "s16e01-kotlin-part1"
        mainClass.set("Part1Kt")
        outputDir = file("build/scripts-part1")
        classpath = tasks
            .named<Jar>("jar")
            .get()
            .outputs.files + configurations.runtimeClasspath.get()
    }

// Create start script for Part 2
val createPart2StartScripts =
    tasks.register<CreateStartScripts>("createPart2StartScripts") {
        applicationName = "s16e01-kotlin-part2"
        mainClass.set("Part2Kt")
        outputDir = file("build/scripts-part2")
        classpath = tasks
            .named<Jar>("jar")
            .get()
            .outputs.files + configurations.runtimeClasspath.get()
    }

// Include additional start scripts in distributions
distributions {
    main {
        contents {
            from(createPart1StartScripts) {
                into("bin")
            }
            from(createPart2StartScripts) {
                into("bin")
            }
        }
    }
}
