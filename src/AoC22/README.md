# Advent of Code 2022 - Haskell Solutions

15 days of Advent of Code 2022 solved in Haskell.

## Solutions

1. **Day 1**: Calorie Counting
2. **Day 2**: Rock Paper Scissors
3. **Day 3**: Rucksack Reorganization
4. **Day 4**: Camp Cleanup
5. **Day 5**: Supply Stacks
6. **Day 6**: Tuning Trouble
7. **Day 7**: No Space Left On Device
8. **Day 8**: Treetop Tree House
9. **Day 9**: Rope Bridge
10. **Day 10**: Cathode-Ray Tube
11. **Day 11**: Monkey in the Middle
12. **Day 12**: Hill Climbing Algorithm
13. **Day 13**: Distress Signal
14. **Day 14**: Regolith Reservoir
15. **Day 15**: Beacon Exclusion Zone

## Building and Running

### Using Bazel (recommended)

```bash
# Build
bazel build //src/AoC22:aoc22

# Run all solutions
bazel run //src/AoC22:aoc22
```

### Using Cabal directly

```bash
# Build
cabal build

# Run all solutions
cabal run aoc22
```

## Project Structure

```
src/AoC22/
├── aoc22.cabal       # Cabal package configuration
├── BUILD.bazel       # Bazel build configuration
├── Main.hs           # Main entry point that runs all days
├── Helpers.hs        # Shared utility functions
├── AoC22E01.hs       # Day 1 solution
├── ...               # Days 2-15
├── AoC22E15.hs       # Day 15 solution
└── data/             # Input files
    ├── data-s22e01-sample.txt
    ├── data-s22e01.txt
    └── ...
```

## Dependencies

- GHC 9.2+ (managed by Bazel)
- Cabal 3.0+ (for direct builds)
- Bazel 7.x+ (recommended, for reproducible builds)

## Output Format

Each solution prints:

- A header with the day and problem title
- Sample results compared against expected values (with ✅/❌)
- Actual results compared against expected values (with ✅/❌)

Example:

```
--- 2022 Day 1: Calorie Counting ---

Max calories sample:                      ✅ 24000
Max calories:                             ✅ 74711
Top 3 total sample:                       ✅ 45000
Top 3 total:                              ✅ 209481
```
