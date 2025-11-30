#!/bin/bash
# Test runner for Swift solution unit tests

set -e

# Find the test binary
TEST_BINARY="$(dirname "$0")/s16e01_swift_test_bin"

# Run the test binary
"$TEST_BINARY"
