#!/usr/bin/env bash
# Helper script to show flake information
# (nix flake show doesn't work due to IFD in Haskell packages)

set -e

SYSTEM="${1:-$(nix eval --impure --expr 'builtins.currentSystem' --raw)}"

echo "🎄 Advent of Code Flake Information"
echo "===================================="
echo ""

echo "📦 Flake Inputs (all connected sub-flakes):"
echo "-------------------------------------------"
nix flake metadata --json | nix run nixpkgs#jq -- -r '.locks.nodes | keys[]' | grep -v "root\|nixpkgs\|flake-utils" | sort
echo ""

echo "✅ Available Checks for $SYSTEM:"
echo "--------------------------------"
nix eval ".#checks.$SYSTEM" --apply builtins.attrNames 2>/dev/null || echo "No checks available for $SYSTEM"
echo ""

echo "📦 Available Packages for $SYSTEM:"
echo "----------------------------------"
nix eval ".#packages.$SYSTEM" --apply builtins.attrNames --impure --allow-import-from-derivation 2>/dev/null || echo "Could not list packages (IFD required)"
echo ""

echo "💡 Useful commands:"
echo "------------------"
echo "  ./show-flake-info.sh         - Show this info"
echo "  nix flake check --verbose    - Run all checks (see what's happening)"
echo "  nix build                    - Build all solution packages"
echo "  nix build .#aoc23            - Build specific package"
echo ""
