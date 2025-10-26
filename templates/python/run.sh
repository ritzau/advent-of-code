#!/usr/bin/env bash
# Run script for Python solution
# Usage: ./run.sh part1|part2 < input.txt

PART=$1

if [ "$PART" = "part1" ]; then
    python3 part1.py
elif [ "$PART" = "part2" ]; then
    python3 part2.py
else
    echo "Usage: $0 part1|part2 < input.txt"
    exit 1
fi
