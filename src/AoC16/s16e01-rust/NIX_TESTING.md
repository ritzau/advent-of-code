# Nix Testing Checklist

This document lists commands to test when Nix is available.

## Prerequisites
```bash
# Enable flakes (if not already enabled)
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

## Core Nix Commands

### Build Package
```bash
nix build
ls -la result/bin/
# Should contain: s16e01, part1, part2
```

### Run Main Verification Binary
```bash
nix run
# Should output:
# AoC 2016 Day 1: No Time for a Taxicab
# ======================================
# Part 1: ✅ 300 (expected: 300) [~8µs]
# Part 2: ✅ 159 (expected: 159) [~200µs]
# Total: ~210µs
# 🌟🌟 All tests passed!
```

### Run Individual Parts with Input
```bash
cat input.txt | nix run .#part1
# Should output: 300

cat input.txt | nix run .#part2
# Should output: 159
```

### Format Code
```bash
nix run .#format
# Should run cargo fmt
```

### Run All Checks (CI)
```bash
nix flake check
# Should run:
# - checks.build (package builds successfully)
# - checks.test (4 unit tests pass)
# - checks.lint (clippy passes with -D warnings)
```

## Justfile Wrapper Commands

The justfile provides convenient shortcuts:

```bash
just build      # -> nix build
just run        # -> nix run
just run-part part1  # -> nix run .#part1
just format     # -> nix run .#format
just check      # -> nix flake check (runs all checks)
just clean      # -> rm -rf target/ result
```

## Flake Structure

```bash
nix flake show
# Should display:
#
# packages.${system}:
#   - default (s16e01 with all 3 binaries)
#
# apps.${system}:
#   - default (main verification binary)
#   - part1 (stdin -> part1 answer)
#   - part2 (stdin -> part2 answer)
#   - format (run cargo fmt)
#
# checks.${system}:
#   - build (package builds)
#   - test (unit tests pass)
#   - lint (clippy passes)
#
# devShells.${system}:
#   - default (cargo, rustc, rustfmt, clippy, rust-analyzer)
```

## Expected Behavior

### Package Build
- `nix build` produces `result/bin/{s16e01,part1,part2}`
- All binaries are statically linked and ready to run

### Apps
- `nix run` runs the main verification and shows timing
- `nix run .#part1` and `.#part2` work with stdin
- `nix run .#format` formats all Rust code

### Checks (CI Integration)
- `nix flake check` runs all validation
- Exit code 0 if all checks pass
- Exit code 1 if any check fails
- All checks run in isolation with proper Rust toolchain

### Justfile
- All `just` commands work without local Rust/Cargo installed
- Everything goes through Nix for reproducibility

## Development Workflow

### Local Development (with devShell)
```bash
nix develop
# Now in shell with cargo, rustc, clippy, rust-analyzer

cargo build     # Fast incremental builds
cargo test      # Fast test iteration
cargo clippy    # Quick linting
```

### CI/Production (pure Nix)
```bash
nix flake check  # Runs all checks
nix build        # Builds release binary
```

### Convenience (with just)
```bash
just check      # CI-style validation
just run        # Quick verification
```

## Troubleshooting

If `nix flake check` fails:

1. **Check Cargo.lock is up to date**
   ```bash
   cargo update
   git add Cargo.lock
   ```

2. **Verify tests pass locally**
   ```bash
   cargo test
   ```

3. **Verify clippy passes**
   ```bash
   cargo clippy -- -D warnings
   ```

4. **Clean and retry**
   ```bash
   just clean
   nix flake check
   ```
