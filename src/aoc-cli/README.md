# AoC Runner

Test infrastructure for Advent of Code solutions - pure Go implementation.

## Features

- Downloads and caches puzzle inputs automatically (pure Go HTTP client)
- Builds solutions using Bazel
- Runs solutions and captures output
- Verifies results against expected values
- Supports running individual days, entire years, or all solutions
- YAML-based configuration for expected results
- No Python dependencies - entirely self-contained Go binary

## Usage

```bash
# Run solutions
aoc s25e03                      # Run day 3 of 2025
aoc s16e01 -l go                # Run day 1 of 2016 (go only)
aoc s16e01 -l go -l rust        # Run multiple languages
aoc 2025                        # Run all days in 2025
aoc --all                       # Run all available solutions

# Auto-detect from directory
cd src/AoC16/s16e01-go && aoc   # Run day 1 (go only)
cd src/AoC16 && aoc             # Run all days in 2016

# Get puzzle input
aoc input s25e03                         # Get input for day 3 of 2025
cd src/AoC25/s25e03-haskell && aoc input # Auto-detect from directory
```

## Results Configuration

Create a `results.yaml` file in the repository root. The format supports both compact array syntax and detailed object syntax:

```yaml
2016:
  1: [300, 159] # Compact: [part1, part2]

  2:
    skip: true # Skip entire day

  3:
    results: [42, 123] # Results with skip options
    skip: ["go"] # Skip only specific languages

  4:
    part1: 100 # Traditional object syntax
    part2: 200
    languages: # Language-specific configuration
      haskell:
        skip: true
```

**Syntax options:**

- **Compact array**: `day: [part1, part2]` - simplest form for expected results
- **Partial results**: `day: [part1]` - only part1 has expected result
- **Empty array**: `day: []` - no expected results yet (will run but not verify)
- **Object with results**: `{results: [part1, part2], skip: ["lang1"]}` - results with selective skip
- **Full object**: `{part1: X, part2: Y, skip: true/[langs], languages: {...}}` - maximum flexibility
- **Skip options**:
  - `skip: true` - skip entire day
  - `skip: ["go", "rust"]` - skip only specific languages
  - `null` in array - run that part but don't verify (e.g., `[part1, null]`)

## Advent of Code Automation Guidelines Compliance

This tool complies with the [Advent of Code automation guidelines](https://www.reddit.com/r/adventofcode/wiki/faqs/automation/):

- ✅ **User-Agent header**: Includes repository URL and contact information
- ✅ **Rate limiting**: Enforces a minimum of 1 minute between requests to adventofcode.com
- ✅ **Input caching**: Downloads each input only once and stores it locally in `inputs/`
- ✅ **Minimal requests**: Only downloads puzzle inputs when needed, no unnecessary API calls

The tool will automatically throttle requests if multiple inputs are downloaded in quick succession.

## Requirements

- Go 1.21 or later
- `.aoc-session` file with your Advent of Code session cookie (see below)
- Bazel 7.x or later for building solutions

### Setting up your session cookie

Create a `.aoc-session` file in the repository root with your session cookie from adventofcode.com:

1. Log in to [Advent of Code](https://adventofcode.com/)
2. Open your browser's developer tools (F12)
3. Go to the Application/Storage tab
4. Find the `session` cookie under Cookies
5. Copy its value to `.aoc-session` in the repository root

Alternatively, set the `AOC_SESSION` environment variable.

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
├── BUILD.bazel      # Bazel build configuration
└── go.mod
```

The `part1` and `part2` binaries must:

- Read input from stdin
- Output only the answer to stdout (no extra formatting)
- Exit with code 0 on success

## Building

```bash
# Build with Bazel
bazel build //src/aoc-cli:aoc

# Run the CLI
bazel run //src/aoc-cli:aoc -- --year 2016 --day 1
```

## Development

```bash
# Run tests
bazel test //src/aoc-cli/...

# Or use Go directly
go test ./...
go fmt ./...
```
