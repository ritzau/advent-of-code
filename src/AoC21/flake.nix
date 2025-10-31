{
  description = "Advent of Code 2021 Solutions in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        package = haskellPackages.callCabal2nix "aoc21" ./. { };

      in
      {
        packages = {
          default = package;
        };

        apps = {
          default = {
            type = "app";
            program = "${package}/bin/aoc21";
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
            echo "🎄 Advent of Code 2021 - Haskell"
            echo ""
            echo "Commands:"
            echo "  cabal build       - Build solution"
            echo "  cabal run aoc21   - Run day 1"
            echo "  ormolu -i *.hs    - Format code"
            echo "  hlint .           - Lint code"
            echo ""
            echo "Nix commands:"
            echo "  nix build         - Build package"
            echo "  nix run           - Run solution"
          '';
        };
      }
    );
}
