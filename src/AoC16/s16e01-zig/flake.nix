{
  description = "Advent of Code 2016 Day 1 solution in Zig";

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
          pname = "s16e01";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = [ pkgs.zig ];

          buildPhase = ''
            runHook preBuild

            zig build-exe main.zig -O ReleaseSafe -femit-bin=s16e01
            zig build-exe part1.zig -O ReleaseSafe -femit-bin=part1
            zig build-exe part2.zig -O ReleaseSafe -femit-bin=part2

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out/bin
            cp s16e01 $out/bin/
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
            name = "s16e01-tests";
            src = ./.;
            nativeBuildInputs = [ pkgs.zig ];
            buildPhase = ''
              zig test part1.zig
              zig test part2.zig
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
            program = "${package}/bin/s16e01";
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
              export PATH=${pkgs.zig}/bin:$PATH
              exec ${pkgs.zig}/bin/zig fmt .
            '');
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
            echo "  zig build-exe main.zig - Build main binary"
            echo "  zig test part1.zig    - Run part1 tests"
            echo "  zig test part2.zig    - Run part2 tests"
            echo "  zig fmt .             - Format code"
            echo ""
            echo "Nix commands:"
            echo "  nix build      - Build package"
            echo "  nix run        - Run verification"
            echo "  nix flake check - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just check     - Run all checks"
            echo "  just run       - Run verification"
          '';
        };
      }
    );
}
