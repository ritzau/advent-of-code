# AoC 2016 Day 1: No Time for a Taxicab (TypeScript)

TypeScript solution for Advent of Code 2016, Day 1.

## Building with Nix

The first time you build, you'll need to update the npm dependencies hash:

1. Run `nix build`
2. It will fail with an error showing the expected hash:
   ```
   error: hash mismatch in fixed-output derivation
   specified: sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
   got:       sha256-<actual-hash-here>
   ```
3. Copy the "got" hash and update `npmDepsHash` in `flake.nix`
4. Run `nix build` again

After the first successful build, the hash is locked and builds will be reproducible.

## Running

```bash
# Build
nix build

# Run verification (reads from stdin)
cat input.txt | nix run

# Run individual parts
echo "R2, L3" | nix run .#part1  # Expected: 5
echo "R8, R4, R4, R8" | nix run .#part2  # Expected: 4
```

## Development

```bash
# Enter dev shell
nix develop

# Install dependencies
npm install

# Compile
tsc

# Run with ts-node
ts-node part1.ts < input.txt
```

## Solution

- **Part 1**: Calculate Manhattan distance to final position (Answer: 300)
- **Part 2**: Find Manhattan distance to first location visited twice (Answer: 159)
