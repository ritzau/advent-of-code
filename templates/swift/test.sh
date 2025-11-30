#!/bin/bash
# Test runner for Swift solution
# Runs the main binary with empty input and checks exit code

set -e

# Find the main binary
MAIN_BINARY="$(dirname "$0")/main"

# Run the binary with empty input
echo "" | "$MAIN_BINARY"
