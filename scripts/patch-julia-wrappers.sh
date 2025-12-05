#!/usr/bin/env bash
# Patch Julia wrapper scripts to enable compiled modules for faster execution
# This removes the --compiled-modules=no flag that bazel's rules_julia adds

set -euo pipefail

echo "Patching Julia wrapper scripts..."

# Find all Julia .sh scripts in bazel-bin
find bazel-bin -name "*.sh" -path "*/julia/*" 2>/dev/null | while read -r script; do
	if grep -q -- "--compiled-modules=no" "$script" 2>/dev/null; then
		# Create a patched version next to the original
		patched="${script%.sh}-fast.sh"

		# Remove the --compiled-modules=no flag
		sed 's/--compiled-modules=no/--compiled-modules=yes/' "$script" >"$patched"
		chmod +x "$patched"

		echo "  Patched: $script -> $patched"
	fi
done

echo "Done patching Julia wrappers"
