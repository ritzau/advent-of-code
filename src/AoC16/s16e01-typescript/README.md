# AoC 2016 Day 1: No Time for a Taxicab (TypeScript)

TypeScript solution for Advent of Code 2016, Day 1.

## Building with Bazel

```bash
# Build
bazel build //src/AoC16/s16e01-typescript:all

# Run individual parts
echo "R2, L3" | bazel run //src/AoC16/s16e01-typescript:part1  # Expected: 5
echo "R8, R4, R4, R8" | bazel run //src/AoC16/s16e01-typescript:part2  # Expected: 4

# Run verification (reads from stdin, validates both parts)
cat input.txt | bazel run //src/AoC16/s16e01-typescript:s16e01-typescript
```

## Development

```bash
# Install dependencies
cd src/AoC16/s16e01-typescript
pnpm install

# Compile
pnpm exec tsc

# Run with ts-node
npx ts-node part1.ts < input.txt
```

## Solution

- **Part 1**: Calculate Manhattan distance to final position (Answer: 300)
- **Part 2**: Find Manhattan distance to first location visited twice (Answer: 159)
