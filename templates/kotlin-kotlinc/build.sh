#!/usr/bin/env bash
# Build Kotlin sources with kotlinc

set -euo pipefail

# Clean previous build
rm -rf classes

# Create output directory
mkdir -p classes

# Compile all Kotlin sources
echo "Compiling Kotlin sources..."
kotlinc -d classes \
    src/main/kotlin/Common.kt \
    src/main/kotlin/Main.kt \
    src/main/kotlin/Part1.kt \
    src/main/kotlin/Part2.kt

echo "Build complete! Classes in: classes/"
