# Advent of Code 2021 Day 1: Sonar Sweep

Haskell solution for AoC 2021 Day 1.

## Problem

- **Part 1**: Count the number of times a depth measurement increases from the previous measurement
- **Part 2**: Count the number of times the sum of measurements in a three-measurement sliding window increases

## Building and Running

### Using Bazel (recommended)

```bash
# Build all parts
bazel build //src/AoC21/s21e01-haskell:all

# Run individual parts
bazel run //src/AoC21/s21e01-haskell:part1 < input.txt
bazel run //src/AoC21/s21e01-haskell:part2 < input.txt

# Run main executable (all parts)
bazel run //src/AoC21/s21e01-haskell:s21e01-haskell < sample.txt
```

### Using Cabal directly

```bash
# Build
cabal build

# Run tests
cabal test

# Run solution
cabal run s21e01-haskell-part1 < input.txt
cabal run s21e01-haskell-part2 < input.txt
```

## Project Structure

```
.
├── Common.hs          # Shared solution logic
├── Part1Main.hs       # Part 1 executable (reads from stdin)
├── Part2Main.hs       # Part 2 executable (reads from stdin)
├── Main.hs            # Combined executable for all parts
├── Tests.hs           # Unit tests
├── aoc-solution.cabal # Cabal configuration
├── BUILD.bazel        # Bazel build configuration
└── sample.txt         # Sample input data
```

## Sample Results

- Part 1: `7`
- Part 2: `5`
