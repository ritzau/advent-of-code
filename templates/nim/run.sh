#!/usr/bin/env bash
# Run script for Nim solution
# Usage: ./run.sh part1|part2 < input.txt

PART=$1

if [ "$PART" = "part1" ]; then
    nim r -d:release --hints:off --verbosity:0 part1.nim
elif [ "$PART" = "part2" ]; then
    nim r -d:release --hints:off --verbosity:0 part2.nim
else
    echo "Usage: $0 part1|part2 < input.txt"
    exit 1
fi
