#!/usr/bin/env bash
# Run script for Rust solution
# Usage: ./run.sh part1|part2 < input.txt

PART=$1

if [ "$PART" = "part1" ]; then
    cargo run --quiet --release --bin part1
elif [ "$PART" = "part2" ]; then
    cargo run --quiet --release --bin part2
else
    echo "Usage: $0 part1|part2 < input.txt"
    exit 1
fi
