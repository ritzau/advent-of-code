# Advent of Code Solutions

Multi-year, multi-language solutions for [Advent of Code](https://adventofcode.com), with a focus on learning and experimentation.

## 🎄 Years

- **[AoC 2025](src/AoC25/)** - 12 languages, 12 days, learning Nix *(in progress)*
- **[AoC 2023](src/AoC23/)** - TypeScript (19/25 days complete)
- **[AoC 2022](src/AoC22/)** - Haskell (15/25 days complete)
- **[AoC 2021](src/AoC21/)** - Haskell (1/25 days complete)

## 🚀 Quick Start (AoC 2025+)

### Prerequisites

- [Nix](https://nixos.org/download.html) installed
- [just](https://github.com/casey/just) command runner (installed automatically in devcontainer)
- Advent of Code session cookie

### Setup

1. **Clone and open in GitHub Codespaces** (or locally with Nix)
2. **Add your session cookie**:
   ```bash
   echo "your_session_cookie_here" > .aoc-session
   ```

3. **Create and solve a day**:
   ```bash
   # Create from template
   just setup 2025 1 python

   # Edit sample input and solution
   vim src/AoC25/day01/sample.txt
   vim src/AoC25/day01/part1.py

   # Test with sample
   just test 2025 1

   # Run with real input
   just run 2025 1
   ```

### Available Templates

- `python` - Python 3
- `typescript` - TypeScript with Node.js
- `rust` - Rust with Cargo
- `go` - Go
- `haskell` - Haskell with Cabal
- `kotlin` - Kotlin scripting
- `nim` - Nim
- `zig` - Zig

Each template includes a Nix flake for reproducible builds.

## 📋 Commands

```bash
just                    # List all available commands
just new 2025 1 python  # Create day from template
just download 2025 1    # Download input
just setup 2025 1 rust  # Create + download
just test 2025 1        # Test with sample
just run 2025 1         # Run with real input
just run-all 2025       # Run all days for a year
just clean-inputs       # Clear input cache
```

## 📁 Structure

```
advent-of-code/
├── src/
│   ├── AoC25/          # 2025 solutions (multi-language, Nix-based)
│   ├── AoC23/          # 2023 solutions (TypeScript)
│   ├── AoC22/          # 2022 solutions (Haskell)
│   └── AoC21/          # 2021 solutions (Haskell)
├── templates/          # Language templates for new days
├── scripts/            # Helper scripts (input downloader)
├── inputs/             # Cached inputs (gitignored)
├── justfile            # Command orchestration
└── .devcontainer/      # GitHub Codespaces config
```

## 🎯 2025 Goals

- **12 languages in 12 days** - One different language per day
- **Learn Nix Flakes** - Each day has its own reproducible Nix flake
- **Minimal dependencies** - Only Nix in devcontainer, languages provided by flakes
- **Simple interface** - All solutions use stdin/stdout
- **GitHub Codespaces** - Develop anywhere with zero local setup

## 🔧 Legacy Solutions

### AoC 2023 (TypeScript)

AoC 2023 includes a Nix flake for reproducible builds using `mkYarnPackage` to respect the upstream yarn.lock file:

```bash
cd src/AoC23

# Using Nix (recommended)
nix develop        # Enter development shell with nodejs_20 and yarn
yarn install       # Install dependencies from yarn.lock
yarn start         # Run all solutions

# Or use just commands
just install       # Install dependencies
just run           # Run all solutions

# Without Nix
yarn install
yarn start
```

The flake uses Node.js 20 (matching upstream) and manages dependencies via yarn.lock for reproducibility.

### AoC 2022 (Haskell)

AoC 2022 has 15 days completed in Haskell with a Nix flake for reproducible builds:

```bash
cd src/AoC22

# Using Nix (recommended)
nix build          # Build the project
nix run            # Run all 15 days

# Using Cabal
cabal build
cabal run aoc22

# Using Just
just run           # Run all solutions
just format        # Format code with ormolu
just lint          # Lint with hlint
```

### AoC 2021 (Haskell)

AoC 2021 has 1 day completed in Haskell with a Nix flake:

```bash
cd src/AoC21

# Using Nix (recommended)
nix build          # Build the project
nix run            # Run day 1

# Using Cabal
cabal build
cabal run aoc21

# Using Just
just run
```

## 📝 Notes

- Inputs are downloaded on demand and cached locally (gitignored)
- Sample inputs from problem descriptions are checked into git
- `flake.lock` files are committed for reproducibility
- Each year may use different approaches as I experiment and learn
- 2025+ uses Nix Flakes for reproducible environments across all languages
- Devcontainer only installs Nix - all language tooling comes from flakes

## 🤝 Contributing

This is a personal learning repository, but feel free to:
- Browse solutions for ideas
- Open issues for discussions
- Suggest improvements to the setup

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.

---

*Happy Advent of Code! 🎄⭐*
