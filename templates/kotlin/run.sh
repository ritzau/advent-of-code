#!/usr/bin/env bash
# Run script for Kotlin solution
# Usage: ./run.sh part1|part2 < input.txt

PART=$1

if [ "$PART" = "part1" ]; then
    kotlinc -script part1.kts
elif [ "$PART" = "part2" ]; then
    kotlinc -script part2.kts
else
    echo "Usage: $0 part1|part2 < input.txt"
    exit 1
fi
