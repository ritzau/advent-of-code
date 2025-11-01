# AoC 2016 Day 1: No Time for a Taxicab (Haskell)

Solution using the Haskell Cabal template.

## Building and Running

### Using Nix (recommended)

```bash
# Build
nix build

# Run tests
just test

# Run part 1
echo "R2, L3" | just run-part 1
# Expected output: 5

# Run part 2
echo "R8, R4, R4, R8" | just run-part 2
# Expected output: 4

# Run all checks
just check
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
