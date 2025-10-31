# Advent of Code 2021 - Haskell Solutions

1 day of Advent of Code 2021 solved in Haskell.

## Solutions

1. **Day 1**: Sonar Sweep

## Building and Running

### Using Nix (recommended)

```bash
# Build
nix build

# Run solution
nix run
```

### Using Cabal directly

```bash
# Build
cabal build

# Run solution
cabal run aoc21
```

### Using Just

```bash
# Build
just build

# Run solution
just run

# Format code
just format

# Lint code
just lint
```

## Project Structure

```
src/AoC21/
├── aoc21.cabal       # Cabal package configuration
├── flake.nix         # Nix flake for reproducible builds
├── justfile          # Command shortcuts
├── Main.hs           # Main entry point
├── Helpers.hs        # Shared utility functions
├── AoC21E01.hs       # Day 1 solution
└── data/             # Input files
    └── data-s21e01-sample.txt
```

## Dependencies

- GHC 9.2+
- Cabal 3.0+
- Nix (optional, for reproducible builds)

## Output Format

The solution prints:
- A header with the day and problem title
- Sample result

Example:
```
--- 2021 Day 1: Sonar Sweep ---

Sample: 7
```
