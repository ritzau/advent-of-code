#!/bin/bash
set -e

# This script is executed after the container is created.

echo "Running post-create script..."

# Install just and curl via Nix
nix profile install nixpkgs#just nixpkgs#curl

# Install nix-direnv for better Nix integration
nix profile install nixpkgs#nix-direnv

# Configure nix-direnv
mkdir -p ~/.config/direnv
cat > ~/.config/direnv/direnvrc << 'EOF'
source $HOME/.nix-profile/share/nix-direnv/direnvrc
EOF

# Add direnv hook to shell configuration files
echo 'eval "$(direnv hook zsh)"' >> ~/.zshrc
echo 'eval "$(direnv hook bash)"' >> ~/.bashrc

# Create .flake-selection with default if it doesn't exist
if [ ! -f .flake-selection ]; then
    echo "templates/python" > .flake-selection
    echo "Created .flake-selection with default: templates/python"
fi

# Allow direnv to load the environment for the workspace
direnv allow .

echo "Post-create script finished."
