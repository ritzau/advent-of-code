#!/usr/bin/env bash
# Force fresh rebuild of all checks without cache
set -e

echo "🔄 Invalidating Nix caches and running fresh checks..."
echo ""

# Show what we're doing
set -x

# Update flake inputs (changes input hashes)
nix flake update

# Run checks with:
# --rebuild: don't use cached check results
# --option eval-cache false: bypass evaluation cache
# --print-build-logs: show full output
nix flake check \
    --rebuild \
    --option eval-cache false \
    --print-build-logs

{ set +x; } 2>/dev/null

echo ""
echo "✅ Fresh checks complete!"
