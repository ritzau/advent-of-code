#!/usr/bin/env bash
# Run Kotlin application with java

set -euo pipefail

# Check if classes exist
if [ ! -d "classes" ]; then
    echo "Error: classes directory not found. Run ./build.sh first."
    exit 1
fi

# Find Kotlin stdlib (common locations)
KOTLIN_STDLIB=""
if [ -f "/usr/share/kotlinc/lib/kotlin-stdlib.jar" ]; then
    KOTLIN_STDLIB="/usr/share/kotlinc/lib/kotlin-stdlib.jar"
elif [ -n "${KOTLIN_HOME:-}" ] && [ -f "${KOTLIN_HOME}/lib/kotlin-stdlib.jar" ]; then
    KOTLIN_STDLIB="${KOTLIN_HOME}/lib/kotlin-stdlib.jar"
fi

if [ -z "$KOTLIN_STDLIB" ]; then
    echo "Error: Could not find kotlin-stdlib.jar. Please set KOTLIN_HOME or ensure kotlinc is installed."
    exit 1
fi

# Run the main verification binary
exec java -cp "classes:${KOTLIN_STDLIB}" MainKt "$@"
