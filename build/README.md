# Custom Bazel Build Rules

This directory contains custom Bazel macros for languages that don't have official rules_* packages or where simpler custom rules are more appropriate.

## Available Rules

### Nim (`nim.bzl`)

Macros for building Nim programs using the Nim compiler.

#### `nim_binary`

Compiles a Nim source file into an executable binary.

**Parameters:**
- `name`: The name of the target (also used as the output binary name)
- `main`: The main .nim source file to compile
- `deps`: List of additional .nim files that the main file depends on (optional)
- `**kwargs`: Additional arguments passed to sh_binary (e.g., visibility)

**Example:**
```python
load("//build:nim.bzl", "nim_binary")

nim_binary(
    name = "my-solution",
    main = "solution.nim",
    deps = ["common.nim"],
    visibility = ["//visibility:public"],
)
```

#### `nim_test`

Compiles and runs a Nim test file.

**Parameters:**
- `name`: The name of the test target
- `main`: The main .nim test file to compile and run
- `deps`: List of additional .nim files that the test file depends on (optional)
- `**kwargs`: Additional arguments passed to sh_test (e.g., size, timeout)

**Example:**
```python
nim_test(
    name = "common_test",
    main = "common.nim",
)
```

### Haskell (`haskell.bzl`)

Macros for building Haskell programs using GHC.

#### `haskell_binary`

Compiles a Haskell source file into an executable binary.

**Parameters:**
- `name`: The name of the target
- `main`: The main .hs source file to compile
- `deps`: List of additional .hs files that the main file depends on (optional)
- `ghc_flags`: List of GHC compiler flags (default: `["-O2"]`)
- `**kwargs`: Additional arguments passed to genrule (e.g., visibility, executable)

**Example:**
```python
load("//build:haskell.bzl", "haskell_binary")

haskell_binary(
    name = "part1",
    main = "Part1.hs",
    deps = ["Common.hs"],
)

# With custom compiler flags
haskell_binary(
    name = "optimized",
    main = "Main.hs",
    deps = ["Common.hs"],
    ghc_flags = ["-O2", "-Wall"],
)
```

#### `haskell_test`

Compiles and runs a Haskell test file.

**Parameters:**
- `name`: The name of the test target
- `main`: The main .hs test file to compile
- `deps`: List of additional .hs files that the test file depends on (optional)
- `ghc_flags`: List of GHC compiler flags (default: `["-O2"]`)
- `**kwargs`: Additional arguments passed to sh_test (e.g., size, timeout)

**Example:**
```python
haskell_test(
    name = "aoc_tests",
    main = "Tests.hs",
    deps = ["Common.hs"],
)
```

## Design Principles

These custom rules follow these principles:

1. **Simplicity**: Hide genrule complexity behind simple, declarative macros
2. **Consistency**: Provide similar interfaces across languages
3. **Flexibility**: Support common customization through parameters
4. **Transparency**: Generate standard Bazel targets (genrule + sh_binary/sh_test)

## Why Custom Rules?

- **Nim**: No official `rules_nim` exists, and Nim's simple compilation model works well with genrules
- **Haskell**: `rules_haskell` exists but is complex for simple builds without external packages. Our custom rules provide:
  - Zero external dependencies
  - Simple GHC-only compilation
  - Easy integration with existing code
  - No need for Cabal/Stack in the build process

## Implementation Notes

Both rule sets use Bazel's `genrule` under the hood:

- **Nim**: Uses `nim compile` with `--nimcache` to isolate build artifacts
- **Haskell**: Uses `ghc` with `-outputdir` and `-O2` optimization by default

Tests use `sh_test` wrappers to integrate with Bazel's test infrastructure.
