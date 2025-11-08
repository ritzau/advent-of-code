{
  description = "Advent of Code 2016 Day 1 solution in Kotlin";

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
          ];

          shellHook = ''
            echo "🎄 Kotlin environment ready (kotlinc-based)"
            echo ""
            echo "Local dev:"
            echo "  ./build.sh             - Build with kotlinc"
            echo "  ./run.sh               - Run application"
            echo "  ./lint.sh              - Lint with ktlint"
            echo "  ktlint -F src/**/*.kt  - Format code"
            echo ""
            echo "Nix commands:"
            echo "  nix build              - Build package"
            echo "  nix run                - Run verification"
            echo "  nix flake check        - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just check             - Run all checks"
            echo "  just run               - Run verification"
          '';
        };
        packages.default = package;

        checks = package.passthru.tests;

        apps = {
          default = {
            type = "app";
            program = "${package}/bin/s16e01-kotlin";
            meta.description = "Run all parts";
          };

          s16e01-kotlin = {
            type = "app";
            program = "${package}/bin/s16e01-kotlin";
            meta.description = "Run all parts";
          };

          s16e01-kotlin-part1 = {
            type = "app";
            program = "${package}/bin/s16e01-kotlin-part1";
            meta.description = "Run part 1";
          };

          s16e01-kotlin-part2 = {
            type = "app";
            program = "${package}/bin/s16e01-kotlin-part2";
            meta.description = "Run part 2";
          };
        };
      });
}
