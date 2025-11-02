# Helper Scripts

## show-flake-info.sh

Shows flake structure information (replacement for `nix flake show` which doesn't work with IFD).

```bash
./scripts/show-flake-info.sh
```

## check-fresh.sh

Runs checks with all caches invalidated - guarantees a fresh rebuild.

```bash
./scripts/check-fresh.sh
```

This script:
1. Updates the flake lock file (invalidates input hashes)
2. Runs checks with `--rebuild` (bypass check cache)
3. Uses `--option eval-cache false` (bypass evaluation cache)
4. Shows full build logs with `--print-build-logs`

## Cache Invalidation Strategies

### Quick: Rebuild checks only
```bash
nix flake check --rebuild
```

### Medium: Bypass evaluation cache
```bash
nix flake check --option eval-cache false
```

### Complete: Update inputs + rebuild
```bash
nix flake update && nix flake check --rebuild
```

### Nuclear: Clear store + rebuild
```bash
nix-collect-garbage -d
nix flake check --rebuild --option eval-cache false
```

## Why Multiple Caches?

Nix has several levels of caching:

1. **Evaluation cache** - Caches the result of evaluating Nix expressions
2. **Check cache** - Remembers which checks passed
3. **Build cache** - Stores built derivations in the Nix store
4. **Flake lock** - Pins input versions (changing this invalidates downstream)

Different flags target different cache levels!
