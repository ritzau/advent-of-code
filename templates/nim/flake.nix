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
          };

          # Run individual parts
          template-nim-part1 = {
            type = "app";
            program = "${package}/bin/template-nim-part1";
          };

          template-nim-part2 = {
            type = "app";
            program = "${package}/bin/template-nim-part2";
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
