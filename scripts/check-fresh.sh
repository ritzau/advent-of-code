#!/usr/bin/env bash
# Force fresh rebuild of all checks without cache
set -e

echo "🔄 Invalidating Nix caches and running fresh checks..."
echo ""

# Show what we're doing
set -x

# Update flake inputs (changes input hashes, forces re-evaluation)
nix flake update

# Run checks with:
# --option eval-cache false: bypass evaluation cache
# --print-build-logs: show full output
nix flake check \
    --option eval-cache false \
    --print-build-logs

{ set +x; } 2>/dev/null

echo ""
echo "✅ Fresh checks complete!"
