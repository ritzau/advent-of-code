# Advent of Code 2021 Day 1: Sonar Sweep

Haskell solution for AoC 2021 Day 1.

## Problem

- **Part 1**: Count the number of times a depth measurement increases from the previous measurement
- **Part 2**: Count the number of times the sum of measurements in a three-measurement sliding window increases

## Building and Running

### Using Nix (recommended)

```bash
# Build
nix build

# Run tests
nix flake check
# or
just check-test

# Run with sample input
nix run < sample.txt

# Run individual parts
nix run .#s21e01-haskell-part1 < input.txt
nix run .#s21e01-haskell-part2 < input.txt

# Or using just
just run part1 < input.txt
just run part2 < input.txt
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
├── flake.nix          # Nix flake for reproducible builds
├── justfile           # Command shortcuts
└── sample.txt         # Sample input data
```

## Sample Results

- Part 1: `7`
- Part 2: `5`
