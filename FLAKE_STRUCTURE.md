# Nix Flake Structure

This repository uses a hierarchical Nix flake structure to manage all Advent of Code solutions across multiple years and languages.

## Hierarchy

```
advent-of-code/
├── flake.nix                    # ROOT FLAKE - Aggregates all years and templates
│
├── src/
│   ├── AoC16/
│   │   ├── flake.nix           # YEAR FLAKE - Aggregates all 2016 days
│   │   ├── s16e01-rust/
│   │   │   └── flake.nix       # DAY FLAKE - Individual solution
│   │   ├── s16e01-python/
│   │   │   └── flake.nix       # DAY FLAKE - Individual solution
│   │   └── ...                 # More day implementations
│   │
│   └── AoC23/
│       └── flake.nix           # YEAR FLAKE - Single flake for all 2023 days
│
└── templates/
    ├── rust/
    │   └── flake.nix           # TEMPLATE FLAKE
    ├── python/
    │   └── flake.nix           # TEMPLATE FLAKE
    └── ...                     # More template flakes
```

## Flake Levels

### 1. Root Flake (`./flake.nix`)

The root flake aggregates all year-level flakes and template flakes. This is the main entry point for CI and comprehensive testing.

**Commands:**
```bash
# Check ALL flakes (years + templates + days)
nix flake check

# Build a specific package
nix build .#aoc16-s16e01-rust
nix build .#aoc23

# Run a specific solution
nix run .#aoc16-s16e01-rust
nix run .#aoc23
```

**Checks:** Runs all checks from all year flakes and template flakes.

### 2. Year Flakes

Each year has a flake that aggregates all day implementations for that year.

#### AoC16 (`src/AoC16/flake.nix`)

Aggregates all 2016 day solutions across multiple languages.

**Commands:**
```bash
cd src/AoC16

# Check all 2016 solutions
nix flake check

# Build specific day/language
nix build .#s16e01-rust

# Run specific day/language
nix run .#s16e01-rust
```

**Checks:** Runs all checks for all day implementations in 2016.

#### AoC23 (`src/AoC23/flake.nix`)

Self-contained flake for all 2023 solutions (TypeScript-based, handles multiple days internally).

**Commands:**
```bash
cd src/AoC23

# Check all 2023 solutions
nix flake check

# Build
nix build

# Run all solutions
nix run

# Run specific day
nix run . -- 1
```

**Checks:** Build, typecheck, and format checks for TypeScript solutions.

### 3. Day Flakes

Individual solution implementations for specific days/languages.

**Example:** `src/AoC16/s16e01-rust/flake.nix`

**Commands:**
```bash
cd src/AoC16/s16e01-rust

# Check this specific solution
nix flake check

# Build
nix build

# Run
nix run

# Development
nix develop
```

**Checks:** Build, test, and lint checks specific to the language (e.g., clippy for Rust).

### 4. Template Flakes

Language templates used to bootstrap new day solutions.

**Example:** `templates/rust/flake.nix`

**Commands:**
```bash
cd templates/rust

# Check template
nix flake check

# Test template build
nix build
```

## Testing All Flakes

### Locally

Use the provided script to check all flakes:

```bash
# Quick check (minimal output)
./scripts/check-all-flakes.sh

# Verbose output
./scripts/check-all-flakes.sh --verbose

# Include individual day checks (more thorough)
./scripts/check-all-flakes.sh --verbose --include-days
```

Or manually at each level:

```bash
# Root: Check everything
nix flake check

# Year level: Check specific year
cd src/AoC16 && nix flake check
cd src/AoC23 && nix flake check

# Day level: Check specific day
cd src/AoC16/s16e01-rust && nix flake check
```

### In CI

The GitHub Actions workflow (`.github/workflows/flake-checks.yml`) automatically:

1. **Checks the root flake** - Validates all flakes together
2. **Checks each year flake** - Parallel jobs for each year
3. **Checks each template flake** - Parallel jobs for each template
4. **Discovers and checks all flakes** - Ensures nothing is missed

The CI runs on:
- Push to main/master branches
- Pull requests
- Manual workflow dispatch

## Adding New Solutions

### Adding a New Day to Existing Year (e.g., AoC16)

1. Create the day solution (use template):
   ```bash
   just new 2016 2 rust  # Day 2, Rust
   ```

2. Update the year flake (`src/AoC16/flake.nix`):
   ```nix
   inputs = {
     # ... existing inputs ...
     s16e02-rust.url = "path:./s16e02-rust";
   };
   ```

3. The root flake automatically picks up changes via the year flake.

### Adding a New Year

1. Create year directory with solutions

2. Create year-level flake (`src/AoCXX/flake.nix`) that:
   - References all day flakes for that year
   - Aggregates their checks and packages

3. Update root flake (`./flake.nix`):
   ```nix
   inputs = {
     # ... existing inputs ...
     aocXX.url = "path:./src/AoCXX";
   };
   ```

4. Update CI workflow to include the new year in the matrix.

## Benefits

✅ **Hierarchical testing** - Test at any level (root, year, day)
✅ **Clear organization** - Logical grouping by year and day
✅ **Parallel CI** - Years and templates tested in parallel
✅ **Automatic aggregation** - Root flake sees all changes
✅ **Granular control** - Work on individual days without affecting others
✅ **Scalable** - Easy to add new years, days, and languages

## Troubleshooting

### Flake lock issues

If you get flake lock errors:

```bash
# Update locks for all flakes
find . -name flake.nix -execdir nix flake lock --update-input nixpkgs \;

# Or update specific flake
cd src/AoC16 && nix flake lock
```

### Path reference issues

If path references don't resolve:

1. Ensure the referenced flake.nix exists
2. Check that the path in `inputs` matches the actual directory structure
3. Try `nix flake lock` to regenerate the lock file

### CI failures

If CI fails but local checks pass:

1. Check that all flake.lock files are committed
2. Ensure no local-only dependencies
3. Run `nix flake check --all-systems` locally to test multi-platform

## See Also

- [NIX_BUILD.md](./NIX_BUILD.md) - General Nix build documentation
- [README.md](./README.md) - Project overview and getting started
- `.github/workflows/flake-checks.yml` - CI workflow configuration
