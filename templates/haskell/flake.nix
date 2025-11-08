{
  description = "Advent of Code solution in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build the Haskell package using Cabal
        haskellPackages = pkgs.haskellPackages;

        # Build package without tests (for faster dev iteration)
        # Prefer a pre-generated static Nix expression if present (generated.nix)
        package = if builtins.pathExists ./generated.nix
          then haskellPackages.callPackage ./generated.nix {}
          else haskellPackages.callCabal2nix "template-haskell" ./. { };

        # Build package with tests enabled (for checks)
        # If we generated a package above, reuse it; otherwise fall back to callCabal2nix
        packageWithTests = pkgs.haskell.lib.compose.doCheck (package);

      in
      {
        packages = {
          default = package;
        };

        checks = {
          # Build succeeds = package is valid
          build = package;

          # Run tests through Nix's Haskell infrastructure
          # This properly handles test dependencies without network access
          test = packageWithTests;

          # Check formatting with ormolu
          format-check = pkgs.stdenv.mkDerivation {
            name = "template-haskell-format-check";
            src = ./.;
            nativeBuildInputs = [ haskellPackages.ormolu ];
            buildPhase = ''
              ormolu --mode check *.hs || (echo "Format check failed. Run 'just format' to fix." && exit 1)
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
            program = "${package}/bin/template-haskell-part1";
            meta.description = "template-haskell: Run part 1";
          };

          # Run individual parts
          template-haskell-part1 = {
            type = "app";
            program = "${package}/bin/template-haskell-part1";
            meta.description = "template-haskell: Run part 1";
          };

          template-haskell-part2 = {
            type = "app";
            program = "${package}/bin/template-haskell-part2";
            meta.description = "template-haskell: Run part 2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.ormolu
            haskellPackages.hlint
          ];

          shellHook = ''
            echo "🎄 Haskell environment ready"
            echo ""
            echo "Local dev:"
            echo "  cabal build         - Build all executables"
            echo "  cabal run part1     - Run part1"
            echo "  cabal test          - Run tests"
            echo "  ormolu -i *.hs      - Format code"
            echo "  hlint .             - Lint code"
            echo ""
            echo "Nix commands:"
            echo "  nix build           - Build package"
            echo "  nix run .#part1     - Run part1"
            echo "  nix run .#part2     - Run part2"
            echo "  nix flake check     - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just check          - Run all checks"
            echo "  just test           - Run tests"
            echo "  just format         - Format code"
            echo "  just format-check   - Check formatting"
          '';
        };
      }
    );
}
