#!/usr/bin/env bash
# Run script for Go solution
# Usage: ./run.sh part1|part2 < input.txt

PART=$1

if [ "$PART" = "part1" ]; then
    go run part1.go common.go
elif [ "$PART" = "part2" ]; then
    go run part2.go common.go
else
    echo "Usage: $0 part1|part2 < input.txt"
    exit 1
fi
