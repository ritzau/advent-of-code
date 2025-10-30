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
    // No dependencies for this simple solution
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "21"
}

application { mainClass.set("MainKt") }
