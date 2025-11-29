# AoC 2016 Day 1: No Time for a Taxicab (Haskell)

Solution using Haskell with Bazel genrule build.

## Building and Running

### Using Bazel (recommended)

```bash
# Build
bazel build //src/AoC16/s16e01-haskell:all

# Run part 1
echo "R2, L3" | bazel run //src/AoC16/s16e01-haskell:part1
# Expected output: 5

# Run part 2
echo "R8, R4, R4, R8" | bazel run //src/AoC16/s16e01-haskell:part2
# Expected output: 4

# Run main executable (both parts with verification)
bazel run //src/AoC16/s16e01-haskell:s16e01-haskell < input.txt
```

### Using Cabal directly

```bash
# Build
cabal build

# Run tests
cabal test

# Run parts
cabal run part1 < sample.txt
cabal run part2 < input.txt
```

## Expected Results

With the actual puzzle input:
- Part 1: 300
- Part 2: 159

## Test Cases

Part 1:
- `R2, L3` → 5 (2 blocks East, 3 blocks North)
- `R2, R2, R2` → 2 (2 blocks South)
- `R5, L5, R5, R3` → 12

Part 2:
- `R8, R4, R4, R8` → 4 (first location visited twice is 4 blocks away)
