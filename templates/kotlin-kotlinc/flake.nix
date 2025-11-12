{
  description = "Advent of Code solution in Kotlin (kotlinc-based)";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable"; };

  inputs.systems.url = "github:nix-systems/default";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-utils.inputs.systems.follows = "systems";

  outputs = { self, systems, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        package = pkgs.callPackage ./build.nix {
          jdk = pkgs.temurin-bin-21;
        };
        jdk = pkgs.temurin-bin-21;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.kotlin
            pkgs.temurin-bin-21
            pkgs.ktlint

            # Common utilities
            pkgs.just      # Command runner
            pkgs.jq        # JSON processing
            pkgs.ripgrep   # Fast search
          ];

          shellHook = ''
            # Mark that we're in this project's Nix shell
            export AOC_NIX_SHELL_ROOT="$PWD"

            echo "🎄 Kotlin environment ready (kotlinc-based)"
            echo ""
            echo "Available commands (auto-detected):"
            echo "  just build        - Build solution"
            echo "  just run [PART]   - Run verification (part1, part2, or default)"
            echo "  just check-test   - Run tests"
            echo "  just check-format - Check formatting"
            echo "  just format       - Format code"
            echo "  just check-all    - Run all checks (hermetic)"
            echo ""
            echo "Environment:"
            echo "  In shell: uses local kotlinc commands (fast)"
            echo "  Outside:  uses nix commands (hermetic)"
            echo "  Set JUST_FORCE_NIX=1 to force Nix mode"
          '';
        };

        packages.default = package;

        checks = package.passthru.tests;

        apps = {
          default = {
            type = "app";
            program = "${package}/bin/template-kotlin-kotlinc";
            meta.description = "template-kotlin-kotlinc: Run all parts";
          };

          template-kotlin-kotlinc = {
            type = "app";
            program = "${package}/bin/template-kotlin-kotlinc";
            meta.description = "template-kotlin-kotlinc: Run all parts";
          };

          template-kotlin-kotlinc-part1 = {
            type = "app";
            program = "${package}/bin/template-kotlin-kotlinc-part1";
            meta.description = "template-kotlin-kotlinc: Run part 1";
          };

          template-kotlin-kotlinc-part2 = {
            type = "app";
            program = "${package}/bin/template-kotlin-kotlinc-part2";
            meta.description = "template-kotlin-kotlinc: Run part 2";
          };
        };
      });
}
