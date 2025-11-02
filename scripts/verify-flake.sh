#!/usr/bin/env bash
# Verify what's built and checked by the flake

set -euo pipefail

SYSTEM="${1:-$(nix eval --impure --raw --expr 'builtins.currentSystem')}"

echo "📦 Packages that will be built:"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
PACKAGES=$(nix eval ".#packages.$SYSTEM" --apply builtins.attrNames --json 2>/dev/null)
echo "$PACKAGES" | jq -r '.[]' | sort
echo ""

PACKAGE_COUNT=$(echo "$PACKAGES" | jq -r '.[]' | wc -l | tr -d ' ')
echo "Total: $PACKAGE_COUNT packages"
echo ""

echo "✅ Checks that will be run:"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
CHECKS=$(nix eval ".#checks.$SYSTEM" --apply builtins.attrNames --json 2>/dev/null)
echo "$CHECKS" | jq -r '.[]' | sort
echo ""

CHECK_COUNT=$(echo "$CHECKS" | jq -r '.[]' | wc -l | tr -d ' ')
echo "Total: $CHECK_COUNT checks"
echo ""

echo "📊 Breakdown by type:"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
TEMPLATES=$(echo "$CHECKS" | jq -r '.[]' | grep -c 'template-' || true)
SOLUTIONS=$(echo "$CHECKS" | jq -r '.[]' | grep -cE 's16e01|aoc2' || true)

echo "  Template checks: $TEMPLATES"
echo "  Solution checks: $SOLUTIONS"
echo ""

echo "🔧 Quick commands:"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  nix build                    - Build all packages"
echo "  nix flake check              - Run all checks"
echo "  nix build .#aoc23            - Build specific package"
echo "  nix build .#checks.$SYSTEM.s16e01-rust-test  - Run specific check"
