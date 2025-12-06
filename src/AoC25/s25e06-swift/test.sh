#!/bin/bash
# Test runner for Swift solution
# Runs the test binary and checks exit code

set -e

# Find the test binary
TEST_BINARY="$(dirname "$0")/test_bin"

# Run the test binary
"$TEST_BINARY"
