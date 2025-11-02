# Migrating Existing Haskell Solutions to the Template

This guide explains how to migrate existing Haskell solutions (from AoC21/AoC22) to the new Cabal template structure.

## Key Differences

### Old Structure (AoC22)
- **Monolithic**: All solutions in one Cabal project
- **IO-based**: Each solution is an `IO ()` action that reads files and prints results
- **File-based input**: Reads from `data/` directory
- **Helpers module**: Shared utilities for printing and file reading
- **Combined parts**: Both parts in one module

### New Template Structure
- **Isolated**: Each day is a separate Cabal project with Nix flake
- **Pure functions**: Core logic is pure, only executables do IO
- **stdin/stdout**: Standard input/output interface
- **HUnit tests**: Test suite with sample cases
- **Separated parts**: part1 and part2 are separate executables

## Migration Steps

### 1. Extract Core Logic

**Old (AoC22E01.hs):**
```haskell
module AoC22E01 (calorieCounting) where

calorieCounting :: IO ()
calorieCounting = do
  printHeader "2022 Day 1: Calorie Counting"
  result <- maxCaloriesOf "data-s22e01.txt"
  printResult "Max calories" 74711 result

maxCaloriesOf :: FilePath -> IO Int
maxCaloriesOf = process maxCalories

parseInput :: String -> [[Int]]
parseInput = ...

maxCalories :: [[Int]] -> Int
maxCalories = foldr (max . sum) 0
```

**New (Common.hs):**
```haskell
module Common (solvePart1, solvePart2) where

solvePart1 :: String -> Int
solvePart1 input =
  let groups = parseInput input
  in maxCalories groups

parseInput :: String -> [[Int]]
parseInput = ...

maxCalories :: [[Int]] -> Int
maxCalories = foldr (max . sum) 0
```

### 2. Create Part Executables

**Part1.hs:**
```haskell
module Main where

import Common (solvePart1)

main :: IO ()
main = do
  input <- getContents
  let result = solvePart1 (trim input)
  print result

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (`elem` " \n\r\t")
```

### 3. Convert Tests

**Old (embedded in solution):**
```haskell
result <- maxCaloriesOf "data-s22e01-sample.txt"
printResult "Max calories sample" 24000 result
```

**New (Tests.hs with HUnit):**
```haskell
allTests :: Test
allTests = TestList
  [ TestLabel "part1_sample" $
      TestCase $ assertEqual "Sample data" 24000 (solvePart1 sampleInput)
  ]

sampleInput :: String
sampleInput = "1000\n2000\n\n3000\n..."
```

### 4. Setup Nix Flake

Copy the template's `flake.nix` and `justfile` to the new solution directory.

## Example: Migrating AoC22E01

### Before
```
src/AoC22/AoC22E01.hs          # Combined solution
data/data-s22e01.txt           # Input file
data/data-s22e01-sample.txt    # Sample input
```

### After
```
src/AoC22/s22e01-haskell/
├── Common.hs                   # Pure logic
├── Part1.hs                    # Part 1 executable
├── Part2.hs                    # Part 2 executable
├── Tests.hs                    # HUnit tests
├── aoc-solution.cabal          # Cabal config
├── flake.nix                   # Nix flake
└── justfile                    # Build commands
```

## Benefits of Migration

1. **Reproducibility**: Nix flakes lock all dependencies
2. **Isolation**: Each day is self-contained
3. **Testability**: HUnit tests run with `cabal test`
4. **Consistency**: Same interface as other language templates
5. **Simplicity**: stdin/stdout is universal

## Bulk Migration

For bulk migration of all AoC22 solutions, consider writing a script that:
1. Extracts the core logic from each module
2. Generates Part1.hs and Part2.hs wrappers
3. Converts printResult calls to HUnit test cases
4. Copies sample data into inline strings
5. Creates directory structure with flake.nix

This is left as an exercise, as the old monolithic structure still works for legacy solutions.
