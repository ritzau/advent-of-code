{
  description = "Advent of Code solution in Zig";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build the Zig package
        package = pkgs.stdenv.mkDerivation {
          pname = "aoc-solution";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = [ pkgs.zig ];

          buildPhase = ''
            runHook preBuild

            zig build-exe part1.zig -O ReleaseSafe -femit-bin=part1
            zig build-exe part2.zig -O ReleaseSafe -femit-bin=part2

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out/bin
            cp part1 $out/bin/
            cp part2 $out/bin/

            runHook postInstall
          '';
        };
      in
      {
        packages = {
          default = package;
        };

        checks = {
          # Build succeeds = package is valid
          build = package;

          # Run tests with proper Zig setup
          test = pkgs.stdenv.mkDerivation {
            name = "aoc-solution-tests";
            src = ./.;
            nativeBuildInputs = [ pkgs.zig ];
            buildPhase = ''
              zig test common.zig
            '';
            installPhase = ''
              mkdir -p $out
              echo "Tests passed" > $out/result
            '';
          };

          # Verify formatting is correct
          format-check = pkgs.stdenv.mkDerivation {
            name = "aoc-solution-format-check";
            src = ./.;
            nativeBuildInputs = [ pkgs.zig ];
            buildPhase = ''
              zig fmt --check .
            '';
            installPhase = ''
              mkdir -p $out
              echo "Format check passed" > $out/result
            '';
          };
        };

        apps = {
          # Default: run part1
          default = {
            type = "app";
            program = "${package}/bin/part1";
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
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            zig
            zls
          ];

          shellHook = ''
            echo "🎄 Zig environment ready"
            echo ""
            echo "Local dev:"
            echo "  zig build-exe part1.zig - Build part1"
            echo "  zig test common.zig     - Run tests"
            echo "  zig fmt .               - Format code"
            echo "  zig fmt --check .       - Check formatting"
            echo ""
            echo "Nix commands:"
            echo "  nix build       - Build package"
            echo "  nix run .#part1 - Run part1"
            echo "  nix run .#part2 - Run part2"
            echo "  nix flake check - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just check        - Run all checks"
            echo "  just test         - Run tests"
            echo "  just format       - Format code"
            echo "  just format-check - Check formatting"
          '';
        };
      }
    );
}
