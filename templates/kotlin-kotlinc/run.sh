#!/usr/bin/env bash
# Run Kotlin application with java

set -euo pipefail

# Check if classes exist
if [ ! -d "classes" ]; then
    echo "Error: classes directory not found. Run ./build.sh first."
    exit 1
fi

# Run the main verification binary
exec java -cp classes MainKt "$@"
