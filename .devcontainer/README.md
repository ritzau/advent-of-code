# Dev Container with Multiple Nix Flakes

This dev container setup allows you to switch between different Nix flakes without restarting the container. It uses direnv with nix-direnv for efficient environment management.

## Setup

The container includes:
- Nix with flakes support
- direnv for automatic environment loading
- nix-direnv for efficient Nix integration with direnv
- VS Code direnv extension for editor integration

## Usage

### Switching Between Flakes

Use the provided script to switch between different flake environments:

```bash
./.devcontainer/switch-flake.sh templates/rust
```

Available flakes in this project:
- `templates/python`
- `templates/rust`
- `templates/go`
- `templates/kotlin`
- `templates/nim`
- `templates/zig`

### After Switching

1. **Terminal**: The environment is automatically reloaded in your current terminal
2. **VS Code**: Reload the window to pick up the new environment
   - Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on Mac)
   - Type "Developer: Reload Window"
   - Press Enter

## How It Works

1. **`.flake-selection`**: This file stores the currently selected flake path
2. **`.envrc`**: Reads the selected flake from `.flake-selection` and uses `use flake` to load it
3. **`switch-flake.sh`**: Updates `.flake-selection` and reloads direnv

The direnv VS Code extension ensures that the editor environment stays in sync with your terminal environment.

## Manual Switching

If you prefer to switch manually:

1. Edit `.flake-selection` to contain the desired flake path (e.g., `templates/rust`)
2. Run `direnv allow .` to reload the environment
3. Reload the VS Code window

## Troubleshooting

### Environment not loading
- Make sure direnv is allowed: `direnv allow .`
- Check that `.flake-selection` contains a valid path
- Verify the flake directory has a `flake.nix` file

### VS Code not picking up changes
- Reload the VS Code window after switching flakes
- Check that the direnv extension is installed and enabled

### Direnv taking a long time
- First load of a flake takes time to build/download dependencies
- Subsequent loads use nix-direnv's cache and are instant
- The cache is stored in `.direnv/` (ignored by git)

## Configuration

### Adding New Flakes

To add a new flake directory:
1. Create a directory with a `flake.nix` file
2. Use the switch script to switch to it: `./.devcontainer/switch-flake.sh path/to/new-flake`

### Customizing the Default

Edit `.flake-selection` to change which flake loads by default when the container starts.
