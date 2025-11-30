# Dev Container for Advent of Code

This dev container provides a consistent development environment for working on Advent of Code solutions.

## Setup

The container includes:

- Bazel for building solutions
- Language-specific tools managed by Bazel
- VS Code extensions for various languages

## Usage

### Building Solutions

All solutions use Bazel for building:

```bash
# Build all solutions
bazel build //...

# Build a specific solution
bazel build //src/AoC16/s16e01-rust:s16e01-rust

# Run a solution
bazel run //src/AoC23:s23e01-part1
```

### Running Tests

```bash
# Run all tests
bazel test //...

# Run tests for a specific solution
bazel test //src/AoC16/s16e01-cpp/tests:solution_test
```

## Language Support

The container supports multiple languages through Bazel's language rules:

- Python (rules_python)
- TypeScript (aspect_rules_ts)
- Rust (rules_rust)
- Go (rules_go)
- Haskell (genrule with GHC)
- Kotlin (rules_kotlin)
- Julia (rules_julia)
- C++ (rules_cc)
- Zig (rules_zig)
- Nim (genrule)

Each language's toolchain is managed by Bazel via MODULE.bazel.
