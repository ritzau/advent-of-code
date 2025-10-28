# Nix Testing Checklist

This document lists commands to test when Nix is available.

## Prerequisites
```bash
# Enable flakes (if not already enabled)
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

## Basic Commands

### Build
```bash
nix build
ls -la result/bin/
# Should contain: s16e01, part1, part2
```

### Run Main Binary
```bash
nix run
# Should output verification with timing
```

### Run Individual Parts
```bash
cat input.txt | nix run .#part1
# Should output: 300

cat input.txt | nix run .#part2
# Should output: 159
```

### Run Tests
```bash
nix run .#test
# Should run cargo test and show 4 passing tests
```

### Run Linter
```bash
nix run .#lint
# Should run clippy with -D warnings
```

### Run Formatter
```bash
nix run .#format
# Should run cargo fmt
```

### Run All Checks (CI)
```bash
nix flake check
# Should run both test and lint checks
```

## Justfile Wrapper Commands

```bash
just build      # -> nix build
just run        # -> nix run
just test       # -> nix run .#test
just lint       # -> nix run .#lint
just format     # -> nix run .#format
just check      # -> nix flake check
just verify     # -> test + lint interactively
```

## Flake Metadata

```bash
nix flake show
# Should display:
# - packages.${system}.default
# - apps.${system}.default
# - apps.${system}.part1
# - apps.${system}.part2
# - apps.${system}.test
# - apps.${system}.lint
# - apps.${system}.format
# - checks.${system}.test
# - checks.${system}.lint
# - devShells.${system}.default
```

## Potential Issues to Watch For

1. **Cargo fetch in checks**: The `checks` derivations might need `cargoLock` configuration
2. **PATH in apps**: The test/lint/format apps might need better PATH setup
3. **Working directory**: Apps using cargo might need explicit `cd` to source directory
4. **Rust toolchain**: Ensure clippy is available in check builds

## Expected Behavior

- `nix build` should produce a working binary in `result/bin/s16e01`
- `nix run` should show test results with emoji and timing
- `nix run .#test` should run all 4 unit tests
- `nix flake check` should pass both test and lint checks
- All just commands should work without local cargo/rust installed
