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

        package = haskellPackages.callCabal2nix "aoc-solution" ./. { };

      in
      {
        packages = {
          default = package;
        };

        checks = {
          # Build succeeds = package is valid
          build = package;

          # Run tests
          test = pkgs.stdenv.mkDerivation {
            name = "aoc-solution-tests";
            src = ./.;
            nativeBuildInputs = [
              haskellPackages.ghc
              haskellPackages.cabal-install
            ];
            buildPhase = ''
              export HOME=$TMPDIR
              cabal update
              cabal test --test-show-details=direct
            '';
            installPhase = ''
              mkdir -p $out
              echo "Tests passed" > $out/result
            '';
          };

          # Check formatting with ormolu
          format-check = pkgs.stdenv.mkDerivation {
            name = "aoc-solution-format-check";
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
