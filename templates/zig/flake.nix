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
            meta.description = "template-zig: Run part 1";
          };

          # Run individual parts
          template-zig-part1 = {
            type = "app";
            program = "${package}/bin/template-zig-part1";
            meta.description = "template-zig: Run part 1";
          };

          template-zig-part2 = {
            type = "app";
            program = "${package}/bin/template-zig-part2";
            meta.description = "template-zig: Run part 2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            # Zig toolchain
            zig
            pkgs.zls

            # Common utilities
            pkgs.just      # Command runner
            pkgs.jq        # JSON processing
            pkgs.ripgrep   # Fast search
          ];

          shellHook = ''
            # Mark that we're in this project's Nix shell
            export AOC_NIX_SHELL_ROOT="$PWD"

            echo "🎄 Zig environment ready"
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
            echo "  In shell: uses local zig commands (fast)"
            echo "  Outside:  uses nix commands (hermetic)"
            echo "  Set JUST_FORCE_NIX=1 to force Nix mode"
          '';
        };
      }
    );
}
