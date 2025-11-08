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
    template-kotlin-gradle = {
      url = "path:./templates/kotlin-gradle";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-kotlin-kotlinc = {
      url = "path:./templates/kotlin-kotlinc";
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
      allProjectFlakes = builtins.filter
        (name: name != "self" && name != "nixpkgs" && name != "flake-utils")
        (builtins.attrNames inputs);

      # Default project flakes - exclude template-kotlin-gradle (heavy Gradle build)
      projectFlakes = builtins.filter
        (name: name != "template-kotlin-gradle")
        allProjectFlakes;

    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Helper to aggregate checks from a list of flakes
        aggregateChecks = flakes: pkgs.lib.foldl'
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
          flakes;

        # Aggregate checks from default project flakes (excluding template-kotlin-gradle)
        allChecks = aggregateChecks projectFlakes;

        # Aggregate checks from ALL project flakes (including template-kotlin-gradle)
        allChecksIncludingGradle = aggregateChecks allProjectFlakes;

        # Helper to determine binary prefix for a flake
        getBinaryPrefix = name:
          if pkgs.lib.hasPrefix "template-" name then
            name  # template-rust -> template-rust
          else if pkgs.lib.hasPrefix "s" name && pkgs.lib.hasInfix "e" name then
            name  # s16e01-rust -> s16e01-rust
          else if name == "aoc21" then
            "aoc21"
          else if name == "aoc22" then
            "aoc22"
          else if name == "aoc23" then
            "aoc23"
          else
            name;

        # Wrap a package to rename its binaries with proper prefix
        wrapWithPrefix = name: pkg:
          let
            prefix = getBinaryPrefix name;
            # Determine if we need to rename binaries
            needsRenaming = (pkgs.lib.hasPrefix "template-" name) ||
                           (pkgs.lib.hasPrefix "s" name && pkgs.lib.hasInfix "e" name);
          in
            if needsRenaming then
              pkgs.symlinkJoin {
                name = "${name}-wrapped";
                paths = [ pkg ];
                postBuild = ''
                  # Rename binaries with prefix
                  if [ -d $out/bin ]; then
                    cd $out/bin
                    # Rename main binary if it exists (various names: aoc-template, s16e01, etc.)
                    # First check if binary already has the correct name
                    if [ -f "${prefix}" ] || [ -L "${prefix}" ]; then
                      # Binary already has correct name (e.g., s16e01-nim, s16e01-zig, s16e01-python), create -main symlink
                      ln -sf "${prefix}" "${prefix}-main"
                    else
                      # Look for binary with old naming
                      for main in aoc-template aoc-solution ${name} s16e01; do
                        if [ -f "$main" ] || [ -L "$main" ]; then
                          ln -sf "$main" "${prefix}-main"
                        fi
                      done
                    fi
                    # Rename part1 and part2 - check both old names and new prefixed names
                    if [ -f "${prefix}-part1" ] || [ -L "${prefix}-part1" ]; then
                      # Binary already has correct name, no additional symlink needed
                      true
                    elif [ -f "part1" ] || [ -L "part1" ]; then
                      ln -sf "part1" "${prefix}-part1"
                    fi
                    if [ -f "${prefix}-part2" ] || [ -L "${prefix}-part2" ]; then
                      # Binary already has correct name, no additional symlink needed
                      true
                    elif [ -f "part2" ] || [ -L "part2" ]; then
                      ln -sf "part2" "${prefix}-part2"
                    fi
                  fi
                '';
              }
            else
              pkg;

        # Helper to aggregate packages from a list of flakes
        aggregatePackages = flakes: pkgs.lib.foldl'
          (acc: name:
            let pkg = inputs.${name}.packages.${system}.default or null;
            in if pkg != null then acc // { ${name} = pkg; } else acc
          )
          {}
          flakes;

        # Aggregate packages from default project flakes (excluding template-kotlin-gradle)
        allPackages = aggregatePackages projectFlakes;

        # Aggregate packages from ALL project flakes (including template-kotlin-gradle)
        allPackagesIncludingGradle = aggregatePackages allProjectFlakes;

        # Aggregate apps from all project flakes
        # Simplified: merge each flake's apps directly, but skip formatter
        # apps and the `default` app, since those are handled elsewhere.
        allApps = pkgs.lib.foldl'
          (acc: name:
            let
              flakeApps = inputs.${name}.apps.${system} or {};
              filteredApps = pkgs.lib.filterAttrs (appName: appValue: appName != "default") flakeApps;
            in
              acc // filteredApps
          )
          {}
          projectFlakes;
      in
      {
        # Default checks exclude template-kotlin-gradle (heavy Gradle build)
        checks = allChecks // {
          # Special check that includes ALL checks including template-kotlin-gradle
          # Can be run with: nix flake check --all-systems --override-input template-kotlin-gradle path:./templates/kotlin-gradle
          # Or build via: nix build .#checks.<system>.all-including-gradle
          all-including-gradle = pkgs.runCommand "all-checks-including-gradle" {
            buildInputs = pkgs.lib.mapAttrsToList (name: check: check) allChecksIncludingGradle;
          } ''
            echo "All checks passed (including template-kotlin-gradle)"
            touch $out
          '';
        };

        packages = allPackages // {
          # Default package that builds all solution packages with renamed binaries (excluding template-kotlin-gradle)
          default = pkgs.symlinkJoin {
            name = "advent-of-code-default";
            paths = pkgs.lib.mapAttrsToList (name: pkg: wrapWithPrefix name pkg) allPackages;
            meta.description = "All Advent of Code solution packages with properly named binaries (excluding template-kotlin-gradle)";
          };

          # Package that builds ALL packages including template-kotlin-gradle
          all = pkgs.symlinkJoin {
            name = "advent-of-code-all";
            paths = pkgs.lib.mapAttrsToList (name: pkg: wrapWithPrefix name pkg) allPackagesIncludingGradle;
            meta.description = "All Advent of Code packages including template-kotlin-gradle";
          };

          # Explicit alias for aoc CLI for easy access
          aoc = inputs.aoc-cli.packages.${system}.default;
        };

        # Apps for easy running
        apps = allApps // {
          # Run the aoc test runner (override default to be aoc)
          # Reference the app or package directly to avoid string-interpolating
          # derivation output paths at evaluation time (which forces builds / IFD).
          aoc = {
            type = "app";
            program = inputs.aoc-cli.apps.${system}.default.program or "${inputs.aoc-cli.packages.${system}.default}/bin/aoc";
            meta.description = "aoc: CLI test runner for all solutions";
          };

          default = {
            type = "app";
            program = inputs.aoc-cli.apps.${system}.default.program or "${inputs.aoc-cli.packages.${system}.default}/bin/aoc";
            meta.description = "aoc: CLI test runner for all solutions";
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
