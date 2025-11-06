{
  description = "Advent of Code solution in Zig";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Use Zig 0.12.x which is stable in nixos-24.05
        zig = pkgs.zig_0_12;

        # Build the Zig package
        package = pkgs.stdenv.mkDerivation {
          pname = "template-zig";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = [ zig ];

          buildPhase = ''
            runHook preBuild

            # Set cache dir to avoid read-only filesystem issues
            export XDG_CACHE_HOME=$TMPDIR/zig-cache

            zig build-exe part1.zig -O ReleaseSafe -femit-bin=template-zig-part1
            zig build-exe part2.zig -O ReleaseSafe -femit-bin=template-zig-part2

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out/bin
            cp template-zig-part1 $out/bin/
            cp template-zig-part2 $out/bin/

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
            name = "template-zig-tests";
            src = ./.;
            nativeBuildInputs = [ zig ];
            buildPhase = ''
              export XDG_CACHE_HOME=$TMPDIR/zig-cache
              zig test common.zig
            '';
            installPhase = ''
              mkdir -p $out
              echo "Tests passed" > $out/result
            '';
          };

          # Verify formatting is correct
          format-check = pkgs.stdenv.mkDerivation {
            name = "template-zig-format-check";
            src = ./.;
            nativeBuildInputs = [ zig ];
            buildPhase = ''
              export XDG_CACHE_HOME=$TMPDIR/zig-cache
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
            program = "${package}/bin/template-zig-part1";
          };

          # Run individual parts
          template-zig-part1 = {
            type = "app";
            program = "${package}/bin/template-zig-part1";
          };

          template-zig-part2 = {
            type = "app";
            program = "${package}/bin/template-zig-part2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            zig
            pkgs.zls
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
