#!/usr/bin/env bash
# Run script for Zig solution
# Usage: ./run.sh part1|part2 < input.txt

PART=$1

if [ "$PART" = "part1" ]; then
    zig run part1.zig
elif [ "$PART" = "part2" ]; then
    zig run part2.zig
else
    echo "Usage: $0 part1|part2 < input.txt"
    exit 1
fi
