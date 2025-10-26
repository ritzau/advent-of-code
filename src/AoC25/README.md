# Advent of Code 2025

This year's focus: **Learning Nix Flakes** by solving AoC problems in different languages, each with its own reproducible Nix environment.

## Philosophy

- **One language per day** - 12 problems, 12 languages
- **Nix Flakes for everything** - Each day has its own `flake.nix` for reproducible builds
- **Minimal devcontainer** - Only Nix installed, all languages provided by flakes
- **Simple interface** - stdin/stdout for all solutions
- **No fancy unification** - Keep each day self-contained

## Quick Start

### Setup Session Cookie

Create a `.aoc-session` file in the repository root with your Advent of Code session cookie:

```bash
echo "your_session_cookie_here" > .aoc-session
```

You can find your session cookie by:
1. Log in to [adventofcode.com](https://adventofcode.com)
2. Open browser dev tools (F12)
3. Go to Application/Storage > Cookies
4. Copy the value of the `session` cookie

### Create a New Day

```bash
# Create from template
just new 2025 1 python

# Or setup (create + download input)
just setup 2025 1 rust
```

Available templates: `python`, `rust`, `go`, `kotlin`, `nim`, `zig`

### Solve the Problem

1. Edit `src/AoC25/day01/sample.txt` with the sample input
2. Implement `part1.{ext}` and `part2.{ext}`
3. Test with sample: `just test 2025 1`
4. Run with real input: `just run 2025 1`

### Run All Solutions

```bash
just run-all 2025
```

## Structure

Each day follows this pattern:

```
day01/
├── flake.nix      # Nix flake defining the environment
├── flake.lock     # Locked dependencies (committed for reproducibility)
├── justfile       # Local commands (run, test, etc.)
├── part1.py       # Part 1 solution (reads stdin, prints result)
├── part2.py       # Part 2 solution (reads stdin, prints result)
├── common.py      # Shared code (optional)
└── sample.txt     # Sample input from problem description
```

## How Solutions Work

### Stdin/Stdout Interface with `just`

All solutions follow a simple stdin/stdout interface using `just`:

```bash
cat input.txt | just run part1
# Output: 54877

cat input.txt | just run part2
# Output: 54100
```

Each day has its own `justfile` with a simple `run` command:

```justfile
# Run a specific part with input from stdin
run PART:
    python3 {{PART}}.py
```

### Nix Flakes

Each day uses Nix flakes for reproducible environments:

```bash
# Enter the environment
cd day01
nix develop

# Now you have Python (or Rust, Go, etc.) available
python3 part1.py < input.txt

# Or run directly without entering the shell
nix develop --command just run part1 < input.txt
```

### Orchestration

The `just` command orchestrates everything:
- Input downloading and caching
- Running solutions in Nix shells
- Timing execution
- Pretty printing results

## Available Commands

```bash
just download 2025 1    # Download input for day 1
just run 2025 1         # Run day 1 (both parts)
just test 2025 1        # Test with sample input
just new 2025 1 python  # Create day 1 from Python template
just setup 2025 1 rust  # Create + download in one step
just run-all 2025       # Run all completed days
just clean-inputs       # Remove all cached inputs
```

## Language Selection

Target languages for 2025 (12 days):
- **Comfortable**: Python, Rust, Go, Kotlin, TypeScript
- **Revisit**: Java, C++, C#
- **New/Learning**: Nim, Zig, and more TBD

Mix it up! Choose based on the problem or what you want to practice.

## Tips

- Keep solutions simple - no fancy frameworks
- Use `common.{ext}` for shared parsing logic
- Sample tests run fast - iterate there first
- Real inputs are cached in `inputs/` (gitignored)

## Example Session

```bash
# Day 1 - Start with Python
$ just setup 2025 1 python
Creating day 01 for 2025 using python template...
✓ Created src/AoC25/day01
Downloading from https://adventofcode.com/2025/day/1/input...
✓ Downloaded and cached: inputs/2025/day01.txt

# Add sample input, implement solution
$ vim src/AoC25/day01/sample.txt
$ vim src/AoC25/day01/part1.py

# Test with sample
$ just test 2025 1
🧪 Testing AoC 2025 Day 1 with sample
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Part 1: 142 (2.3ms)
Part 2: Not yet implemented

# Implement part 2
$ vim src/AoC25/day01/part2.py

# Run with real input
$ just run 2025 1
🎄 Advent of Code 2025 - Day 1
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Part 1: 54877 (12.3ms)
Part 2: 54100 (15.6ms)
Total: 27.9ms
```

Happy coding! 🎄
