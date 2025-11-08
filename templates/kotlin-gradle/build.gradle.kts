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
    kotlin("jvm") version "2.2.21"
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
    compilerOptions {
        jvmTarget.set(org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_21)
    }
}

application {
    mainClass.set("MainKt")
    applicationName = "template-kotlin-gradle"
}

// Don't generate Windows batch files
tasks.withType<CreateStartScripts> {
    doLast {
        delete(windowsScript)
    }
}
