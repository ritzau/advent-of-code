# Nix Build Support (Optional)

All language templates (Rust, Go, Kotlin, Zig, Nim, Python) now support building directly with Nix for maximum reproducibility.

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

Uses `buildGoModule` with standard Go project layout:

```nix
packages = {
  default = pkgs.buildGoModule {
    pname = "aoc-solution";
    version = "0.1.0";
    src = ./.;

    vendorHash = null; # No external dependencies

    subPackages = [ "cmd/part1" "cmd/part2" ];
  };
};
```

**Project structure:**
```
cmd/
  part1/
    main.go
  part2/
    main.go
common/
  common.go
go.mod
```

**Requirements:**
- `go.mod` - defines module name
- `cmd/` subdirectories - standard Go layout
- `vendorHash = null` - for projects without external dependencies

**Build output:**
- `result/bin/part1`
- `result/bin/part2`

**Note:** The template uses the standard Go project layout with `cmd/` subdirectories. This is idiomatic Go and works seamlessly with `buildGoModule`, nixpkgs' official Go builder. The `subPackages` parameter tells Nix which binaries to build.

### Kotlin

Uses `buildBazelPackage` with Bazel and rules_kotlin:

```nix
packages = {
  default = pkgs.buildBazelPackage {
    pname = "aoc-solution";
    version = "0.1.0";
    src = ./.;

    bazel = pkgs.bazel_7;

    bazelTargets = [ "//:part1" "//:part2" ];

    fetchAttrs = {
      sha256 = "..."; # Hash of fetched dependencies
    };

    buildAttrs = {
      installPhase = ''
        mkdir -p $out/bin
        cp bazel-bin/part1 $out/bin/
        cp bazel-bin/part2 $out/bin/
      '';
    };
  };
};
```

**Project structure:**
```
BUILD.bazel     # Bazel build definitions
WORKSPACE       # Bazel workspace with rules_kotlin
part1.kt
part2.kt
common.kt
```

**Requirements:**
- `BUILD.bazel` - defines `kt_jvm_binary` targets
- `WORKSPACE` - configures rules_kotlin (v1.9.0)
- `fetchAttrs.sha256` - hash of fetched Bazel dependencies

**Build output:**
- `result/bin/part1`
- `result/bin/part2`

**Note:** Bazel provides hermetic, reproducible builds with excellent caching. The template uses `buildBazelPackage` which separates dependency fetching (fetch phase) from building (build phase), ensuring full reproducibility. While more complex than simple kotlinc, Bazel scales well and teaches modern build system concepts.

### Zig

Uses `stdenv.mkDerivation` with Zig compiler:

```nix
packages = {
  default = pkgs.stdenv.mkDerivation {
    pname = "aoc-solution";
    version = "0.1.0";
    src = ./.;

    nativeBuildInputs = [ pkgs.zig ];

    buildPhase = ''
      zig build-exe part1.zig -O ReleaseSafe -femit-bin=part1
      zig build-exe part2.zig -O ReleaseSafe -femit-bin=part2
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp part1 $out/bin/
      cp part2 $out/bin/
    '';
  };
};
```

**Project structure:**
```
part1.zig
part2.zig
```

**Requirements:**
- Simple standalone Zig files
- No external dependencies or build.zig needed for AoC solutions

**Build output:**
- `result/bin/part1`
- `result/bin/part2`

**Note:** Zig's simple compilation model works perfectly with `mkDerivation`. For projects with dependencies managed by Zig's package manager, consider using zig2nix, but for AoC's standalone programs, direct compilation is cleaner.

### Nim

Uses `buildNimPackage` following nixpkgs best practices:

```nix
packages = {
  default = pkgs.buildNimPackage (finalAttrs: {
    pname = "aoc-solution";
    version = "0.1.0";
    src = ./.;

    lockFile = ./lock.json;

    nimbleFile = ./aoc_solution.nimble;

    nimFlags = [ "-d:NimblePkgVersion=${finalAttrs.version}" ];
  });
};
```

**Project structure:**
```
aoc_solution.nimble   # Nimble package definition
lock.json             # Dependency lockfile
part1.nim
part2.nim
common.nim
```

**Requirements:**
- `aoc_solution.nimble` - Nimble package file defining binaries and dependencies
- `lock.json` - Lockfile generated by `nim_lk` utility (empty for projects without dependencies)
- Nim source files with local imports

**Build output:**
- `result/bin/part1`
- `result/bin/part2`

**Note:** `buildNimPackage` is the recommended approach for Nim projects in nixpkgs. It uses lockfiles for reproducible builds and supports Nimble package metadata. For projects without external dependencies, the lockfile is simply `{"depends": [], "tasks": {}}`. The `nim_lk` utility can generate lockfiles for projects with dependencies. An alternative is `buildNimSbom` which uses CycloneDX SBOM files instead of lockfiles.

### Python

Uses `stdenv.mkDerivation` with wrapper scripts:

```nix
packages = {
  default = pkgs.stdenv.mkDerivation {
    pname = "aoc-solution";
    version = "0.1.0";
    src = ./.;

    buildInputs = [ pkgs.python3 ];

    installPhase = ''
      mkdir -p $out/bin $out/lib
      cp *.py $out/lib/

      # Create wrapper scripts
      cat > $out/bin/part1 <<EOF
      #!/bin/sh
      exec ${pkgs.python3}/bin/python3 $out/lib/part1.py "\$@"
      EOF

      chmod +x $out/bin/part1
      # ... (similar for part2)
    '';
  };
};
```

**Project structure:**
```
part1.py
part2.py
common.py
```

**Requirements:**
- Pure Python with standard library only
- No external pip packages for AoC solutions

**Build output:**
- `result/bin/part1` (wrapper script)
- `result/bin/part2` (wrapper script)
- `result/lib/*.py` (Python source files)

**Note:** Python is interpreted, so "building" just creates wrapper scripts that invoke Python with the correct paths. For projects with pip dependencies, use `buildPythonApplication` or `buildPythonPackage` from nixpkgs.

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

# Kotlin
cd src/AoC25/day03
nix build  # First run will fail with hash mismatch - copy the correct hash
# Update fetchAttrs.sha256 in flake.nix with the correct hash
nix build
cat sample.txt | ./result/bin/part1

# Zig
cd src/AoC25/day04
nix build
cat sample.txt | ./result/bin/part1

# Nim
cd src/AoC25/day05
nix build
cat sample.txt | ./result/bin/part1

# Python (interpreted, no build artifacts)
cd src/AoC25/day06
nix build
cat sample.txt | ./result/bin/part1
```

## Complexity Assessment

**Added complexity:**
- ✅ Minimal for users (optional feature)
- ✅ No changes to justfile needed
- ✅ Dev shell workflow unchanged
- ⚠️  Requires Cargo.lock (Rust), go.mod (Go), BUILD.bazel + WORKSPACE (Kotlin)
- ⚠️  Users need to understand two build methods
- ⚠️  Kotlin requires computing dependency hash on first build

**Benefits:**
- ✅ True reproducibility for those who want it
- ✅ Aligns with Nix philosophy
- ✅ Can be ignored by users who don't care
- ✅ Kotlin/Bazel teaches modern build systems

## Recommendation

**For most users:** Stick with `just build` - it's simpler and faster.

**For reproducibility enthusiasts:** Use `nix build` when you need guarantees.

Both approaches coexist peacefully - the dev shell is still the primary development environment.
