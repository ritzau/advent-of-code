# Advent of Code Solutions

Multi-year, multi-language solutions for [Advent of Code](https://adventofcode.com), with a focus on learning and experimentation.

## 🎄 Years

- **[AoC 2025](src/AoC25/)** - 12 languages, 12 days *(in progress)*
- **[AoC 2023](src/AoC23/)** - TypeScript (19/25 days complete)
- **[AoC 2022](src/AoC22/)** - Haskell (15/25 days complete)
- **[AoC 2021](src/AoC21/)** - Haskell (1/25 days complete)
- **[AoC 2016](src/AoC16/)** - 10 languages, 1 day

## 🚀 Quick Start

### Prerequisites

**Required:**
- [Bazel](https://bazel.build/) 7.x or later (or [Bazelisk](https://github.com/bazelbuild/bazelisk) for automatic version management)
- [GHC](https://www.haskell.org/ghc/) (Haskell compiler) - for Haskell solutions
- [Nim](https://nim-lang.org/) compiler - for Nim solutions

**Optional:**
- [just](https://github.com/casey/just) command runner - for convenience commands
- Advent of Code session cookie - for automatic input downloading

**Note:** All other language toolchains (Go, Rust, TypeScript/Node.js, Kotlin, Julia, Zig, C++, .NET) are automatically managed by Bazel via [MODULE.bazel](MODULE.bazel).

### Setup

1. **Clone the repository**

2. **Add your session cookie** (optional, for downloading inputs):
   ```bash
   echo "your_session_cookie_here" > .aoc-session
   ```

3. **Download puzzle inputs** (optional):
   ```bash
   # Download input for a specific day
   bazel run //src/aoc-cli:aoc -- download -y 2016 -d 1
   ```

4. **Build and run solutions**:
   ```bash
   # Build all solutions
   bazel build //...

   # Build a specific solution (all parts)
   bazel build //src/AoC16/s16e01-rust:all

   # Run a solution part
   echo "R2, L3" | bazel run //src/AoC16/s16e01-rust:part1
   echo "R8, R4, R4, R8" | bazel run //src/AoC16/s16e01-rust:part2

   # Run main verification binary (validates both parts)
   bazel run //src/AoC16/s16e01-rust:s16e01-rust < input.txt

   # Run tests
   bazel test //...
   ```

### Available Templates

- `python` - Python 3.13
- `typescript` - TypeScript with Node.js 25
- `rust` - Rust 1.91
- `go` - Go 1.25
- `haskell` - Haskell (GHC with custom build rules)
- `kotlin` - Kotlin JVM
- `nim` - Nim (with custom build rules)
- `zig` - Zig 0.15
- `cpp` - C++ with CMake
- `julia` - Julia 1.11

All templates include Bazel BUILD files for reproducible builds.

## 📋 Building

```bash
# Build everything
bazel build //...

# Build specific year
bazel build //src/AoC23:all
bazel build //src/AoC16/...

# Build specific solution
bazel build //src/AoC16/s16e01-rust:all
bazel build //src/AoC22:aoc22

# Run tests
bazel test //src/AoC16/s16e01-cpp/tests:solution_test
bazel test //templates/rust:all

# Run a solution
bazel run //src/AoC16/s16e01-rust:s16e01-rust < input.txt
```

## 🔧 AoC CLI

The repository includes a custom CLI tool for downloading inputs, building solutions, and running tests:

```bash
# Run all solutions for a specific day
bazel run //src/aoc-cli:aoc -- run -y 2016 -d 1

# Run a specific language implementation
bazel run //src/aoc-cli:aoc -- run -y 2016 -d 1 -l rust

# Download puzzle input
bazel run //src/aoc-cli:aoc -- download -y 2016 -d 1

# Show help
bazel run //src/aoc-cli:aoc -- --help
```

The CLI automatically:
- Finds all language implementations for a given day
- Builds solutions using Bazel
- Runs both parts and validates results
- Displays execution times and success/failure status

## 📁 Structure

```
advent-of-code/
├── src/
│   ├── AoC25/          # 2025 solutions (multi-language)
│   ├── AoC23/          # 2023 solutions (TypeScript)
│   ├── AoC22/          # 2022 solutions (Haskell)
│   ├── AoC21/          # 2021 solutions (Haskell)
│   ├── AoC16/          # 2016 solutions (10 languages)
│   └── aoc-cli/        # CLI test runner (Go)
├── templates/          # Language templates for new solutions
├── build/              # Custom Bazel rules (nim.bzl, haskell.bzl)
├── scripts/            # Helper scripts
├── MODULE.bazel        # Bazel module configuration
├── BUILD.bazel         # Root BUILD file
└── justfile            # Command orchestration (optional)
```

## 🎯 Build System

This repository uses **Bazel** for reproducible builds across all languages:

- **Hermetic builds** - All dependencies managed by Bazel
- **Cross-language support** - Unified build system for 10+ languages
- **Caching** - Fast incremental builds
- **Toolchain management** - Specific language versions pinned in MODULE.bazel

### Language-Specific Notes

**TypeScript (AoC 2023)**:
- Uses `aspect_rules_ts` and `aspect_rules_js`
- pnpm for dependency management
- 19 days with part1 and part2 binaries

**Haskell (AoC 2021, AoC 2022)**:
- Uses custom `haskell_binary` and `haskell_test` macros (see `build/haskell.bzl`)
- Simple builds without external package dependencies
- AoC22: Single executable for all 15 days
- AoC21: Three executables (main, part1, part2)

**Rust (AoC 2016)**:
- Uses `rules_rust` with Cargo
- Rust 1.91 toolchain

**Julia (AoC 2016)**:
- Uses `rules_julia`
- Julia 1.11.2 toolchain
- Proper library and binary rules

**Other Languages**:
- Go: `rules_go` with gazelle
- Python: `rules_python` with pip
- C++: `rules_cc` with native toolchain
- Kotlin: `rules_kotlin` with JVM
- Zig: `rules_zig` 0.15.2
- Nim: Custom `nim_binary` and `nim_test` macros (see `build/nim.bzl`)

## 📝 Notes

- Inputs are downloaded on demand and cached locally (gitignored)
- Sample inputs from problem descriptions are checked into git
- All solutions use stdin/stdout for I/O
- Each language may use different approaches for experimentation

## 🤝 Contributing

This is a personal learning repository, but feel free to:
- Browse solutions for ideas
- Open issues for discussions
- Suggest improvements to the setup

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.

---

*Happy Advent of Code! 🎄⭐*
