#!/usr/bin/env bash
#
# Check all flakes in the repository
# Usage: ./scripts/check-all-flakes.sh [--verbose]
#

set -e

VERBOSE=false
if [[ "$1" == "--verbose" ]]; then
  VERBOSE=true
fi

echo "🎄 Advent of Code - Checking All Flakes"
echo "========================================"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

failed=0
total=0

# Function to check a single flake
check_flake() {
  local dir="$1"
  local name="$2"

  total=$((total + 1))

  echo -n "📦 Checking $name... "

  if $VERBOSE; then
    echo ""
    echo "========================================"
  fi

  if (cd "$dir" && nix flake check --show-trace 2>&1 | if $VERBOSE; then cat; else cat > /dev/null; fi); then
    echo -e "${GREEN}✅ PASSED${NC}"
  else
    echo -e "${RED}❌ FAILED${NC}"
    failed=$((failed + 1))
  fi

  if $VERBOSE; then
    echo "========================================"
  fi
  echo ""
}

# Check root flake (aggregates everything)
echo "🌟 Checking Root Flake (All Solutions)"
echo "========================================"
check_flake "." "Root (all years, all templates)"

# Check year-level flakes
echo ""
echo "📅 Checking Year-Level Flakes"
echo "========================================"
if [ -f "src/AoC16/flake.nix" ]; then
  check_flake "src/AoC16" "AoC 2016 (all days)"
fi
if [ -f "src/AoC23/flake.nix" ]; then
  check_flake "src/AoC23" "AoC 2023"
fi

# Check template flakes
echo ""
echo "📝 Checking Template Flakes"
echo "========================================"
for template_dir in templates/*/; do
  if [ -f "$template_dir/flake.nix" ]; then
    template_name=$(basename "$template_dir")
    check_flake "$template_dir" "Template: $template_name"
  fi
done

# Check individual day flakes (optional, for granular testing)
if [[ "$2" == "--include-days" ]]; then
  echo ""
  echo "📆 Checking Individual Day Flakes"
  echo "========================================"

  for day_dir in src/AoC16/s16e*/; do
    if [ -f "$day_dir/flake.nix" ]; then
      day_name=$(basename "$day_dir")
      check_flake "$day_dir" "Day: $day_name"
    fi
  done
fi

# Summary
echo ""
echo "========================================"
echo "📊 Summary"
echo "========================================"
echo "Total checked: $total"
echo -e "Passed: ${GREEN}$((total - failed))${NC}"
if [ $failed -gt 0 ]; then
  echo -e "Failed: ${RED}${failed}${NC}"
  echo ""
  echo -e "${RED}❌ Some checks failed${NC}"
  exit 1
else
  echo -e "Failed: ${GREEN}0${NC}"
  echo ""
  echo -e "${GREEN}✅ All checks passed!${NC}"
fi
