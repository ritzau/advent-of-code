# Helper Scripts

## show-flake-info.sh

Shows flake structure information (replacement for `nix flake show` which doesn't work with IFD).

```bash
./scripts/show-flake-info.sh
```

## check-fresh.sh

Runs checks with caches invalidated - forces fresh evaluation and rebuild.

```bash
./scripts/check-fresh.sh
```

This script:
1. Updates the flake lock file (invalidates input hashes, forces re-evaluation)
2. Uses `--option eval-cache false` (bypass evaluation cache)
3. Shows full build logs with `--print-build-logs`

## Cache Invalidation Strategies

### Method 1: Update flake inputs (Most Reliable)
```bash
nix flake update          # Updates lock file, invalidates input hashes
nix flake check           # Re-evaluates and rebuilds
```

### Method 2: Bypass evaluation cache
```bash
nix flake check --option eval-cache false
```

### Method 3: Clear Nix store (Nuclear)
```bash
nix-collect-garbage -d   # Delete all unreferenced store paths
nix flake check          # Everything rebuilds from scratch
```

### Method 4: Commit changes (Git-based cache)
```bash
git add -A && git commit -m "Invalidate cache"
nix flake check          # Flakes cache based on git tree hash
```

### Method 5: Combined approach (Strongest)
```bash
nix flake update && nix flake check --option eval-cache false --print-build-logs
```

## Why Multiple Caches?

Nix has several levels of caching:

1. **Evaluation cache** - Caches the result of evaluating Nix expressions
   - Bypassed with: `--option eval-cache false`

2. **Git tree cache** - Flakes are cached by git commit hash
   - Invalidated by: Committing changes or `nix flake update`

3. **Build cache** - Stores built derivations in `/nix/store`
   - Invalidated by: `nix-collect-garbage` or changing inputs

4. **Flake lock** - Pins input versions
   - Invalidated by: `nix flake update`

**Note:** `nix flake check` doesn't have a `--rebuild` flag (that's only for `nix build`)!
