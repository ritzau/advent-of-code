#!/usr/bin/env bash
# Script to switch between different Nix flakes in the dev container
set -e

FLAKE_SELECTION_FILE=".flake-selection"

# Function to display usage
usage() {
    echo "Usage: $0 <flake-directory>"
    echo ""
    echo "Switch to a different Nix flake environment."
    echo ""
    echo "Available flakes:"
    find templates -maxdepth 1 -type d -not -name templates | sort | sed 's/^/  - /'
    echo ""
    echo "Example:"
    echo "  $0 templates/rust"
    echo "  $0 templates/python"
    exit 1
}

# Check if an argument was provided
if [ $# -eq 0 ]; then
    usage
fi

FLAKE_DIR="$1"

# Validate that the flake directory exists
if [ ! -d "$FLAKE_DIR" ]; then
    echo "Error: Directory '$FLAKE_DIR' does not exist"
    exit 1
fi

# Validate that the directory contains a flake.nix
if [ ! -f "$FLAKE_DIR/flake.nix" ]; then
    echo "Error: No flake.nix found in '$FLAKE_DIR'"
    exit 1
fi

# Save the selection
echo "$FLAKE_DIR" > "$FLAKE_SELECTION_FILE"
echo "Switched to flake: $FLAKE_DIR"

# Reload direnv
echo "Reloading direnv environment..."
direnv allow .

echo ""
echo "Environment switched successfully!"
echo ""
echo "For VS Code to pick up the new environment:"
echo "  1. Press Ctrl+Shift+P (or Cmd+Shift+P on Mac)"
echo "  2. Type 'Developer: Reload Window'"
echo "  3. Press Enter"
echo ""
echo "Your terminal environment has been updated."
