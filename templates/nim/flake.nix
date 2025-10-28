{
  description = "Advent of Code solution in Nim";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build the Nim package
        package = pkgs.buildNimPackage (finalAttrs: {
          pname = "aoc-solution";
          version = "0.1.0";
          src = ./.;

          lockFile = ./lock.json;

          nimbleFile = ./aoc_solution.nimble;

          nimFlags = [ "-d:NimblePkgVersion=${finalAttrs.version}" ];
        });
      in
      {
        packages = {
          default = package;
        };

        checks = {
          # Build succeeds = package is valid
          build = package;

          # Run tests with proper Nim setup
          test = pkgs.stdenv.mkDerivation {
            name = "aoc-solution-tests";
            src = ./.;
            buildInputs = [ pkgs.nim ];
            buildPhase = ''
              export HOME=$TMPDIR
              nim c -r common.nim
            '';
            installPhase = ''
              mkdir -p $out
              echo "Tests passed" > $out/result
            '';
          };
        };

        apps = {
          # Default: run main verification binary
          default = {
            type = "app";
            program = "${package}/bin/aoc-solution";
          };

          # Run individual parts
          part1 = {
            type = "app";
            program = "${package}/bin/part1";
          };

          part2 = {
            type = "app";
            program = "${package}/bin/part2";
          };

          # Format code (app because it modifies files)
          format = {
            type = "app";
            program = toString (pkgs.writeShellScript "format" ''
              export PATH=${pkgs.nim}/bin:$PATH
              exec ${pkgs.nim}/bin/nimpretty *.nim
            '');
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nim
            nimlsp
          ];

          shellHook = ''
            echo "🎄 Nim environment ready"
            echo ""
            echo "Local dev:"
            echo "  nim c -r common.nim  - Run tests"
            echo "  nim c part1.nim      - Build part1"
            echo "  nimpretty *.nim      - Format code"
            echo ""
            echo "Nix commands:"
            echo "  nix build            - Build package"
            echo "  nix run              - Run verification"
            echo "  nix flake check      - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just check           - Run all checks"
            echo "  just run             - Run verification"
          '';
        };
      }
    );
}
