# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Multi-year, multi-language solutions for Advent of Code with a focus on learning and experimentation. The repository uses Nix flakes for reproducible builds across all languages and years.

**Years:**
- **AoC16** - 2016 Day 1 solutions (all 8 languages)
- **AoC21** - Haskell (1/25 days)
- **AoC22** - Haskell (15/25 days)
- **AoC23** - TypeScript (19/25 days)
- **AoC25** - Multi-language learning project (in progress)

## Architecture

### Root-Level Flake Orchestration

The root `flake.nix` aggregates all subflakes:
- **Template flakes** (`template-*`) - Language scaffolding, excluded from package builds
- **Solution flakes** - Individual year/day implementations
- All solution packages are aggregated into a `default` package using `symlinkJoin`
- Checks are aggregated from all flakes (including templates) for CI validation

### Template System

Each language template in `templates/` is a standalone Nix flake providing:
- Development environment with language toolchain
- Build system configuration (Cargo, go.mod, Bazel, etc.)
- Justfile for common operations
- Checks for formatting, linting, and tests

Templates use the root flake's nixpkgs input (except Zig, which pins nixos-24.05 for `zig_0_12`).

### Solution Structure

Each day follows the pattern:
```
src/AoCXX/dayNN/
├── flake.nix          # Nix environment (references root template inputs)
├── justfile           # Local commands (run, build, test, format)
├── sample.txt         # Sample input from problem description
├── part1.<ext>        # Part 1 solution
├── part2.<ext>        # Part 2 solution
└── common.<ext>       # Shared code (optional)
```

Solutions read from stdin and write to stdout for simplicity.

## Common Commands

### Creating New Solutions

```bash
# Create from template
just new 2025 1 python

# Create + download real input (requires .aoc-session file)
just setup 2025 1 rust

# Download input for existing day
just download 2025 1
```

### Running Solutions

```bash
# Test with sample input
just test 2025 1

# Run with real input (downloads if missing)
just run 2025 1

# Run all days for a year
just run-all 2025
```

### Working Within a Solution Directory

```bash
cd src/AoC25/day01

# Build (using Nix dev shell)
just build

# Run specific part with custom input
echo "input" | just run part1

# Format code
just format

# Run all checks (build + tests + lint)
just check-all
```

### Nix Build System

Two approaches coexist:

**Dev Shell Build (default, fast):**
```bash
cd src/AoC25/day01
just build                          # Uses nix develop --command
```

**Pure Nix Build (fully reproducible):**
```bash
cd src/AoC25/day01
nix build                           # Creates ./result/bin/part1 and part2
cat sample.txt | ./result/bin/part1
```

Root-level operations:
```bash
# Build all solution packages
nix build

# Check all templates and solutions (runs their checks)
nix flake check --verbose

# Check with fresh rebuild (invalidates caches)
./scripts/check-fresh.sh

# Show flake structure (replaces nix flake show)
./scripts/show-flake-info.sh

# List all checks
nix eval .#checks.x86_64-darwin --apply builtins.attrNames  # macOS
nix eval .#checks.x86_64-linux --apply builtins.attrNames   # Linux

# Build specific solution
nix build .#aoc23
```

### Legacy Solutions

**AoC 2023 (TypeScript):**
```bash
cd src/AoC23
nix develop        # Enter shell with Node.js 20 and yarn
yarn install
yarn start
```

**AoC 2022/2021 (Haskell):**
```bash
cd src/AoC22
nix build          # Or nix run
just format        # Format with ormolu
just lint          # Lint with hlint
```

## Available Language Templates

- `python` - Python 3 with pytest, ruff (format/lint)
- `typescript` - TypeScript with Node.js, tsx, vitest
- `rust` - Rust with Cargo
- `go` - Go with standard layout (`cmd/` subdirectories)
- `haskell` - Haskell with Cabal
- `kotlin` - Kotlin with Bazel and rules_kotlin
- `nim` - Nim with Nimble
- `zig` - Zig 0.12 (pins nixos-24.05)

## Important Implementation Details

### Stdin/Stdout Interface

All solutions follow a consistent interface:
- Read input from stdin
- Write answer to stdout
- Part 1 and Part 2 are separate executables

This allows the root justfile to uniformly run any solution regardless of language.

### Input Management

- `.aoc-session` file (gitignored) contains session cookie for downloading inputs
- `scripts/download-input.py` downloads inputs to `inputs/YEAR/dayNN.txt` (cached, gitignored)
- `sample.txt` files are checked into git
- The root justfile automatically downloads real inputs when running solutions

### Flake Dependencies

- Root flake provides shared nixpkgs and flake-utils inputs
- All templates and solutions follow these inputs (except Zig)
- Template flakes are referenced as path inputs (`path:./templates/<lang>`)
- Solution flakes reference templates via root flake inputs

### Build Reproducibility

The repository demonstrates progressive levels of reproducibility:
- **Dev shell builds** - Consistent toolchain, fast iteration
- **Pure Nix builds** - Hermetic, cacheable, bit-for-bit reproducible
- **Lockfiles** - flake.lock, Cargo.lock, go.mod, yarn.lock committed

### Nix Caching Gotchas

Nix flakes are cached by git tree hash. To invalidate caches:
1. Commit changes: `git add -A && git commit`
2. Update flake: `nix flake update`
3. Use `--option eval-cache false`
4. Run `./scripts/check-fresh.sh` (combines methods)

**Note:** `nix flake check` doesn't have a `--rebuild` flag (that's only for `nix build`).

## Session Cookie Setup

To download real puzzle inputs:

```bash
# Get session cookie from browser (logged into adventofcode.com)
echo "your_session_cookie_here" > .aoc-session
```

The cookie is used by `scripts/download-input.py` and should never be committed.

## Testing Strategy

When modifying templates or solutions:

1. **Template validation** - `nix flake check` runs checks for all templates
2. **Individual solution tests** - `cd src/AoCXX/dayNN && just check-all`
3. **Fresh rebuild** - `./scripts/check-fresh.sh` to verify without caching
4. **Verify aggregation** - `nix build` to ensure all solutions build together

## Language-Specific Notes

**Rust** - Uses `rustPlatform.buildRustPackage`, requires Cargo.lock

**Go** - Standard layout with `cmd/part1/main.go` and `cmd/part2/main.go`, uses `buildGoModule`

**Kotlin** - Uses Bazel with rules_kotlin v1.9.0, requires computing dependency hash on first build

**Zig** - Simple `mkDerivation` with direct compilation, no build.zig needed for AoC

**Nim** - Uses `buildNimPackage` with lockfiles, empty lockfile for projects without dependencies

**Python** - Wrapper scripts calling python3 interpreter, uses pytest and ruff

**TypeScript** - tsx for execution, vitest for testing

**Haskell** - Cabal-based with ormolu (format) and hlint (lint)
