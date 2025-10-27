#!/bin/bash
set -e

# This script is executed after the container is created.

echo "Running post-create script..."

# Install just and curl via Nix
nix profile install nixpkgs#just nixpkgs#curl

# Allow direnv to load the environment for the workspace
direnv allow .

# Add direnv hook to shell configuration files
echo 'eval "$(direnv hook zsh)"' >> ~/.zshrc
echo 'eval "$(direnv hook bash)"' >> ~/.bashrc

echo "Post-create script finished."
