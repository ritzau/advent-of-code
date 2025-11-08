#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
cd "$repo_root"

echo "Searching for flake.nix files that use callCabal2nix..."
# Find files but exclude any under .direnv or hidden folders
mapfile -t dirs < <(grep -RIl "callCabal2nix" src templates 2>/dev/null | grep -v "/\.direnv/" | grep -v "/\.git/" || true)

if [ ${#dirs[@]} -eq 0 ]; then
  echo "No Haskell flakes with callCabal2nix found."
  exit 0
fi

echo "Found ${#dirs[@]} files. Generating generated.nix in each directory using cabal2nix..."

for f in "${dirs[@]}"; do
  dir=$(dirname "$f")
  echo "- Generating in $dir"
  # Run cabal2nix inside the directory using nix-shell to provide cabal2nix
  (cd "$dir" && nix-shell -p cabal2nix --run "cabal2nix . > generated.nix")
  echo "  -> $dir/generated.nix created"
done

echo "Done. Next steps:"
echo "  - Inspect each generated.nix and commit it to the repo." 
echo "  - Update the corresponding flake.nix to prefer the generated file. Example change:" 
cat <<'EOF'
# BEFORE
package = haskellPackages.callCabal2nix "aoc-solution" ./. { };

# AFTER (prefer generated.nix)
package = if builtins.pathExists ./generated.nix
  then import ./generated.nix
  else haskellPackages.callCabal2nix "aoc-solution" ./. { };
EOF

echo "If you want I can update the flake.nix files to use the generated.nix automatically — tell me and I'll apply the edits and run quick checks."
