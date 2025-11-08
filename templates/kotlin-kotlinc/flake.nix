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
