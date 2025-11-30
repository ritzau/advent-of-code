# Swift Template

Swift template for Advent of Code solutions using Bazel.

## Platform Support

This template supports both **macOS** and **Linux** platforms:

- ✅ **macOS**: Works out of the box with Xcode Command Line Tools
- ✅ **Linux**: Requires Swift toolchain installation and `CC=clang` environment variable

## Linux Requirements

To use this template on Linux:

1. **Install Swift toolchain**: Download from [swift.org](https://swift.org/download/) for your distribution
2. **Install dependencies**: Ensure ICU and Clang are installed
3. **Set environment variable**: Run Bazel with `CC=clang` because rules_swift expects clang as the linking driver:
   ```bash
   CC=clang bazel build //templates/swift:part1
   ```
4. **PATH configuration**: Bazel will use the first `swift` executable found in your PATH

## Template Features

- ✅ Uses `@main` attribute (Swift 5.3+)
- ✅ Proper module structure with public functions
- ✅ Cross-platform support (macOS and Linux)
- ✅ No hardcoded platform-specific compiler flags
- ✅ Uses `Date()` for cross-platform timing (works on both macOS and Linux)
- ✅ Apple CC toolchain configured in MODULE.bazel

## Building

Build any target:

```bash
bazel build //templates/swift:part1
bazel build //templates/swift:part2
bazel build //templates/swift:main
```

## Testing

Run the test suite:

```bash
bazel test //templates/swift:aoc_test
```

The test runs the `main` binary which validates both parts of the solution and exits with code 0 on success or 1 on failure.
