{
  description = "Advent of Code Templates";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Template flakes
    template-go = {
      url = "path:./templates/go";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-haskell = {
      url = "path:./templates/haskell";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-kotlin = {
      url = "path:./templates/kotlin";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-nim = {
      url = "path:./templates/nim";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-python = {
      url = "path:./templates/python";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-rust = {
      url = "path:./templates/rust";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-typescript = {
      url = "path:./templates/typescript";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-zig = {
      url = "path:./templates/zig";
      # Don't follow nixpkgs - Zig template uses its own pinned version (24.05) for zig_0_12
      inputs.flake-utils.follows = "flake-utils";
    };

    # Legacy Haskell solutions
    aoc21 = {
      url = "path:./src/AoC21";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    aoc22 = {
      url = "path:./src/AoC22";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    aoc23 = {
      url = "path:./src/AoC23";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    aoc-cli = {
      url = "path:./src/aoc-cli";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    # 2016 Day 1 Solutions (multi-language implementations)
    s16e01-go = {
      url = "path:./src/AoC16/s16e01-go";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-haskell = {
      url = "path:./src/AoC16/s16e01-haskell";
      # Don't follow root nixpkgs - this solution uses nixos-24.05 for GHC 9.6.5
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-kotlin = {
      url = "path:./src/AoC16/s16e01-kotlin";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-nim = {
      url = "path:./src/AoC16/s16e01-nim";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-python = {
      url = "path:./src/AoC16/s16e01-python";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-rust = {
      url = "path:./src/AoC16/s16e01-rust";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-typescript = {
      url = "path:./src/AoC16/s16e01-typescript";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-zig = {
      url = "path:./src/AoC16/s16e01-zig";
      # Don't follow nixpkgs - Zig uses its own pinned version for zig_0_12
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    let
      # All project flakes - everything except self and system inputs
      projectFlakes = builtins.filter
        (name: name != "self" && name != "nixpkgs" && name != "flake-utils")
        (builtins.attrNames inputs);

    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Aggregate checks from all project flakes (including templates)
        # Namespace each flake's checks to avoid name collisions
        allChecks = pkgs.lib.foldl'
          (acc: name:
            let
              flakeChecks = inputs.${name}.checks.${system} or {};
              # Prefix each check with the flake name
              namespacedChecks = pkgs.lib.mapAttrs'
                (checkName: checkValue: pkgs.lib.nameValuePair "${name}-${checkName}" checkValue)
                flakeChecks;
            in
              acc // namespacedChecks
          )
          {}
          projectFlakes;

        # Aggregate packages from all project flakes (including templates)
        # Don't wrap them here to avoid IFD issues with nix flake show
        allPackages = pkgs.lib.foldl'
          (acc: name:
            let pkg = inputs.${name}.packages.${system}.default or null;
            in if pkg != null then acc // { ${name} = pkg; } else acc
          )
          {}
          projectFlakes;

        # Aggregate apps from all project flakes
        # Namespace each flake's apps to avoid name collisions
        allApps = pkgs.lib.foldl'
          (acc: name:
            let
              flakeApps = inputs.${name}.apps.${system} or {};
              # For the default app, use the flake name directly
              # For other apps, prefix with flake name (e.g., s16e01-go-part1)
              namespacedApps = pkgs.lib.mapAttrs'
                (appName: appValue:
                  if appName == "default" then
                    pkgs.lib.nameValuePair name appValue
                  else
                    pkgs.lib.nameValuePair "${name}-${appName}" appValue
                )
                flakeApps;
            in
              acc // namespacedApps
          )
          {}
          projectFlakes;
      in
      {
        checks = allChecks;

        packages = allPackages // {
          # Default package that builds all solution packages
          default = pkgs.symlinkJoin {
            name = "advent-of-code-all";
            paths = pkgs.lib.attrValues allPackages;
            meta.description = "All Advent of Code solution packages";
          };

          # Explicit alias for aoc CLI for easy access
          aoc = inputs.aoc-cli.packages.${system}.default;
        };

        # Apps for easy running
        apps = allApps // {
          # Run the aoc test runner (override default to be aoc)
          aoc = {
            type = "app";
            program = "${inputs.aoc-cli.packages.${system}.default}/bin/aoc";
          };

          default = {
            type = "app";
            program = "${inputs.aoc-cli.packages.${system}.default}/bin/aoc";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            git
            just
            nixpkgs-fmt
          ] ++ [
            inputs.aoc-cli.packages.${system}.default
          ];

          shellHook = ''
            echo "🎄 Advent of Code Templates"
            echo ""
            echo "Available templates:"
            echo "  Go, Haskell, Kotlin, Nim, Python, Rust, TypeScript, Zig"
            echo ""
            echo "Solutions:"
            echo "  src/AoC16/s16e01-*         - 2016 Day 1 (all 8 languages)"
            echo "  src/AoC21                 - 2021 solutions (Haskell)"
            echo "  src/AoC22                 - 2022 solutions (Haskell)"
            echo "  src/AoC23                 - 2023 solutions (TypeScript)"
            echo ""
            echo "AoC Test Runner (runs all languages):"
            echo "  aoc --year 2016 --day 1    - Run all language implementations for a day"
            echo "  aoc -y 2016 -d 1           - Same, with short flags"
            echo "  aoc --year 2016            - Run all days in a year"
            echo "  aoc --all                  - Run all available solutions"
            echo ""
            echo "Run specific solutions:"
            echo "  nix run .#s16e01-go        - Run Go solution (with pretty output)"
            echo "  nix run .#s16e01-go-part1  - Run Go part1 only (raw output)"
            echo "  nix run .#s16e01-rust      - Run Rust solution"
            echo "  cat inputs/2016/day01.txt | nix run .#s16e01-go-part1  - Pipe input"
            echo ""
            echo "Build specific solutions:"
            echo "  nix build .#aoc            - Build the aoc CLI"
            echo "  nix build .#s16e01-go      - Build specific solution"
            echo ""
            echo "Or use aoc CLI via Nix:"
            echo "  nix run .#aoc -- -y 2016 -d 1  - Run the aoc CLI via Nix"
            echo "  nix run . -- -y 2016 -d 1  - Same (aoc is default app)"
            echo ""
            echo "Commands:"
            echo "  nix flake check --verbose    - Check all templates and solutions (shows what's running)"
            echo "  nix flake check --print-build-logs - Show full build output for failing checks"
            echo "  nix build                    - Build all solution packages (default)"
            echo "  nix build .#aoc23            - Build specific solution package"
            echo "  cd templates/<lang>          - Work on specific template"
            echo ""
            echo "Verify what checks are included:"
            echo "  nix eval .#checks.x86_64-darwin --apply builtins.attrNames  # List all checks (macOS)"
            echo "  nix eval .#checks.x86_64-linux --apply builtins.attrNames   # List all checks (Linux)"
            echo "  nix flake metadata           # Show flake inputs without evaluating"
            echo ""
            echo "Helper scripts:"
            echo "  scripts/show-flake-info.sh   - Show flake structure (replaces nix flake show)"
            echo "  scripts/check-fresh.sh       - Run checks with fresh rebuild (no cache)"
            echo ""
            echo "Just commands:"
            echo "  just new <year> <day> <lang> - Create new solution from template"
            echo ""
          '';
        };
      }
    );
}
