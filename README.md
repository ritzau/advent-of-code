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
- `rust` - Rust with Cargo
- `go` - Go
- `kotlin` - Kotlin scripting
- `nim` - Nim
- `zig` - Zig

Each template includes a Nix shell for reproducible builds.

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
- **Learn Nix** - Each day has its own reproducible Nix environment
- **Simple interface** - All solutions use stdin/stdout
- **GitHub Codespaces** - Develop anywhere with zero local setup

## 🔧 Legacy Solutions

### AoC 2023 (TypeScript)

```bash
cd src/AoC23
yarn install
yarn start
```

### AoC 2022 (Haskell)

```bash
cabal run
```

## 📝 Notes

- Inputs are downloaded on demand and cached locally (gitignored)
- Sample inputs from problem descriptions are checked into git
- Each year may use different approaches as I experiment and learn
- 2025+ uses Nix for reproducible environments across all languages

## 🤝 Contributing

This is a personal learning repository, but feel free to:
- Browse solutions for ideas
- Open issues for discussions
- Suggest improvements to the setup

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.

---

*Happy Advent of Code! 🎄⭐*
