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
          pname = "template-nim";
          version = "0.1.0";
          src = ./.;

          lockFile = ./lock.json;

          nimbleFile = ./template_nim.nimble;

          nimFlags = [ "-d:NimblePkgVersion=${finalAttrs.version}" ];

                    # Rename binaries from underscores to dashes
          postInstall = ''
            mv $out/bin/template_nim $out/bin/template-nim
            mv $out/bin/template_nim_part1 $out/bin/template-nim-part1
            mv $out/bin/template_nim_part2 $out/bin/template-nim-part2
          '';
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
            name = "template_nim_tests";
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

          # Verify formatting is correct
          format-check = pkgs.stdenv.mkDerivation {
            name = "template-nim-format-check";
            src = ./.;
            buildInputs = [ pkgs.nim pkgs.diffutils ];
            buildPhase = ''
              # Copy nim files to temp location to check formatting
              cp *.nim $TMPDIR/
              cd $TMPDIR
              nimpretty *.nim
              # Compare formatted files with originals
              for file in *.nim; do
                if ! diff -q "$file" "$src/$file" > /dev/null; then
                  echo "Format check failed for $file. Run 'just format' to fix."
                  exit 1
                fi
              done
            '';
            installPhase = ''
              mkdir -p $out
              echo "Format check passed" > $out/result
            '';
          };
        };

        apps = {
          # Default: run main verification binary
          default = {
            type = "app";
            program = "${package}/bin/template-nim";
            meta.description = "template-nim: Run all parts";
          };

          # Run individual parts
          template-nim-part1 = {
            type = "app";
            program = "${package}/bin/template-nim-part1";
            meta.description = "template-nim: Run part 1";
          };

          template-nim-part2 = {
            type = "app";
            program = "${package}/bin/template-nim-part2";
            meta.description = "template-nim: Run part 2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Nim toolchain
            nim
            nimlsp

            # Common utilities
            just      # Command runner
            jq        # JSON processing
            ripgrep   # Fast search
          ];

          shellHook = ''
            # Mark that we're in this project's Nix shell
            export AOC_NIX_SHELL_ROOT="$PWD"

            echo "🎄 Nim environment ready"
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
            echo "  In shell: uses local nim commands (fast)"
            echo "  Outside:  uses nix commands (hermetic)"
            echo "  Set JUST_FORCE_NIX=1 to force Nix mode"
          '';
        };
      }
    );
}
