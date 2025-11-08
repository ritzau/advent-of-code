# Kotlin Template (kotlinc-based)

A simple Advent of Code template using `kotlinc` and `ktlint` for Kotlin solutions, designed to work seamlessly with Nix builds.

## Features

- **Simple build system**: Uses `kotlinc` directly, no Gradle overhead
- **Nix-friendly**: Works perfectly with Nix flakes and reproducible builds
- **Fast compilation**: Direct compilation with minimal dependencies
- **Linting**: Integrated `ktlint` for code quality

## Directory Structure

```
.
├── src/
│   ├── main/kotlin/
│   │   ├── Common.kt   # Shared code
│   │   ├── Main.kt     # Verification runner
│   │   ├── Part1.kt    # Part 1 solution
│   │   └── Part2.kt    # Part 2 solution
│   └── test/kotlin/
│       └── CommonTest.kt  # Tests
├── build.sh            # Build script (kotlinc)
├── run.sh              # Run script (java)
├── lint.sh             # Lint script (ktlint)
├── build.nix           # Nix build definition
├── flake.nix           # Nix flake
└── sample.txt          # Sample input
```

## Local Development

### With Nix (Recommended)

```bash
# Enter development shell
nix develop

# Build
./build.sh

# Run
./run.sh < sample.txt

# Lint
./lint.sh

# Format
ktlint -F src/**/*.kt
```

### Without Nix

Requires: Java 21+, kotlinc, ktlint

```bash
# Build
./build.sh

# Run
./run.sh < sample.txt

# Lint
./lint.sh
```

## Nix Commands

```bash
# Build package
nix build

# Run verification
nix run < sample.txt

# Run part 1 only
nix run .#template-kotlin-kotlinc-part1 < sample.txt

# Run part 2 only
nix run .#template-kotlin-kotlinc-part2 < sample.txt

# Run all checks (build + tests + lint)
nix flake check

# Show all outputs
nix flake show
```

## Comparison with kotlin-gradle Template

### kotlin-kotlinc (this template)
- **Pros**: Simple, fast, Nix-friendly, no Gradle complexity
- **Cons**: Manual dependency management, no advanced build features

### kotlin-gradle
- **Pros**: Powerful build system, dependency management, mature ecosystem
- **Cons**: Complex Nix integration, slower builds, more dependencies

Choose `kotlin-kotlinc` for simple AoC solutions. Choose `kotlin-gradle` for larger projects with dependencies.
