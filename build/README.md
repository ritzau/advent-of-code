# Custom Bazel Build Rules

This directory contains custom Bazel macros for languages that don't have official rules\_\* packages or where simpler custom rules are more appropriate.

## Available Rules

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

**Haskell**: While `rules_haskell` exists, it is complex for simple builds without external packages. Our custom rules provide:

- Zero external dependencies
- Simple GHC-only compilation
- Easy integration with existing code
- No need for Cabal/Stack in the build process

## Implementation Notes

The Haskell rules use Bazel's `genrule` under the hood:

- Uses `ghc` with `-outputdir` and `-O2` optimization by default
- Tests use `sh_test` wrappers to integrate with Bazel's test infrastructure

## Officially Supported Languages

For other languages, use the official Bazel rules:

- **Nim**: Use `@rules_nim` (see MODULE.bazel)
- **C/C++**: Use `@rules_cc`
- **Rust**: Use `@rules_rust`
- **Go**: Use `@rules_go`
- **Python**: Use `@rules_python`
- **TypeScript**: Use `@aspect_rules_ts`
- **Kotlin**: Use `@rules_kotlin`
- **Zig**: Use `@rules_zig`
- **Julia**: Use `@rules_julia`
- **C#**: Use `@rules_dotnet`
- **Swift**: Use `@rules_swift`
