# AoC Runner

Test infrastructure for Advent of Code solutions - pure Go implementation.

## Features

- Downloads and caches puzzle inputs automatically (pure Go HTTP client)
- Builds solutions using Nix (with fallback to `go build`)
- Runs solutions and captures output
- Verifies results against expected values
- Supports running individual days, entire years, or all solutions
- YAML-based configuration for expected results
- No Python dependencies - entirely self-contained Go binary

## Usage

```bash
# Run a specific day
aoc-runner -year 2016 -day 1

# Run all days in a year
aoc-runner -year 2016

# Run all available solutions
aoc-runner -all

# Use a custom results file
aoc-runner -year 2016 -day 1 -results path/to/results.yaml
```

## Results Configuration

Create a `results.yaml` file in the repository root:

```yaml
2016:
  1:
    part1: 300
    part2: 159
  2:
    skip: true  # Skip this day
  3:
    part1: null  # No expected result yet (will run but not verify)
    part2: null
```

## Requirements

- Go 1.21 or later
- `.aoc-session` file with your Advent of Code session cookie
- (Optional) Nix with flakes enabled for reproducible builds

## Solution Structure

Solutions must follow this structure:

```
src/AoC16/s16e01-go/
├── cmd/
│   ├── part1/
│   │   └── main.go  # Reads from stdin, outputs only the result
│   └── part2/
│       └── main.go  # Reads from stdin, outputs only the result
├── common/
│   └── common.go    # Shared solution code
├── main.go          # Optional: pretty output with timing
├── flake.nix        # Nix build configuration
└── go.mod
```

The `part1` and `part2` binaries must:
- Read input from stdin
- Output only the answer to stdout (no extra formatting)
- Exit with code 0 on success

## Building

```bash
# Build with Nix
nix build

# Or build with Go directly
go build
```

## Development

```bash
# Enter development shell
nix develop

# Run tests
go test ./...

# Format code
go fmt ./...
```
