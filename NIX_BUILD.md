# Nix Build Support (Optional)

The Rust and Go templates now support building directly with Nix for maximum reproducibility.

## Two Build Approaches

### Approach 1: Dev Shell Build (Default)

This is the approach used by `just build`:

```bash
cd src/AoC25/day01

# Enter dev shell and build
nix develop --command cargo build --release

# Or just use justfile
just build
```

**Pros:**
- Simple and familiar
- Works like normal cargo/go commands
- Fast incremental builds

**Cons:**
- Builds in dev shell environment
- Less reproducible

### Approach 2: Pure Nix Build (New)

Build using Nix's package system:

```bash
cd src/AoC25/day01

# Build using pure Nix
nix build

# Binaries are in ./result/bin/
./result/bin/part1 < input.txt
./result/bin/part2 < input.txt
```

**Pros:**
- **Fully reproducible** - bit-for-bit identical builds
- **Pure** - no reliance on dev shell state
- **Cacheable** - Nix can cache and share builds
- **Hermetic** - isolated from system state

**Cons:**
- Slower (no incremental builds)
- More complex
- Requires understanding Nix

## Language-Specific Details

### Rust

Uses `rustPlatform.buildRustPackage`:

```nix
packages = {
  default = pkgs.rustPlatform.buildRustPackage {
    pname = "aoc-solution";
    version = "0.1.0";
    src = ./.;
    cargoLock.lockFile = ./Cargo.lock;
  };
};
```

**Requirements:**
- `Cargo.toml` - already present in template
- `Cargo.lock` - already present in template
- Both binaries (part1, part2) built automatically

**Build output:**
- `result/bin/part1`
- `result/bin/part2`

### Go

Uses custom `stdenv.mkDerivation`:

```nix
packages = {
  default = pkgs.stdenv.mkDerivation {
    pname = "aoc-solution";
    version = "0.1.0";
    src = ./.;
    nativeBuildInputs = [ pkgs.go ];
    buildPhase = ''
      go build -o part1 part1.go common.go
      go build -o part2 part2.go common.go
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp part1 $out/bin/
      cp part2 $out/bin/
    '';
  };
};
```

**Requirements:**
- `go.mod` - present in template (minimal)
- No external dependencies needed

**Build output:**
- `result/bin/part1`
- `result/bin/part2`

**Note:** We use `stdenv.mkDerivation` instead of `buildGoModule` because the AoC template structure (multiple main packages at root) doesn't fit Go's typical cmd/ subdirectory pattern.

## When to Use Each Approach

### Use Dev Shell Build (just build) when:
- Developing and iterating quickly
- Making frequent changes
- Don't need reproducibility guarantees
- Want familiar build commands

### Use Pure Nix Build (nix build) when:
- Need fully reproducible builds
- Submitting solutions (ensure it works everywhere)
- Building for distribution
- Want to cache builds
- Learning Nix

## Testing Nix Build

To verify nix build works:

```bash
# Rust
cd src/AoC25/day01
nix build
cat sample.txt | ./result/bin/part1

# Go
cd src/AoC25/day02
nix build
cat sample.txt | ./result/bin/part1
```

## Complexity Assessment

**Added complexity:**
- ✅ Minimal for users (optional feature)
- ✅ No changes to justfile needed
- ✅ Dev shell workflow unchanged
- ⚠️  Requires Cargo.lock (Rust) and go.mod (Go) in templates
- ⚠️  Users need to understand two build methods

**Benefits:**
- ✅ True reproducibility for those who want it
- ✅ Aligns with Nix philosophy
- ✅ Can be ignored by users who don't care

## Recommendation

**For most users:** Stick with `just build` - it's simpler and faster.

**For reproducibility enthusiasts:** Use `nix build` when you need guarantees.

Both approaches coexist peacefully - the dev shell is still the primary development environment.
