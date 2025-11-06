{
  description = "Advent of Code 2022 Solutions in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        # Prefer a generated static expression when available
        package = if builtins.pathExists ./generated.nix
          then haskellPackages.callPackage ./generated.nix {}
          else haskellPackages.callCabal2nix "aoc22" ./. { };

      in
      {
        packages = {
          default = package;
        };

        apps = {
          default = {
            type = "app";
            program = "${package}/bin/aoc22";
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
            echo "🎄 Advent of Code 2022 - Haskell"
            echo ""
            echo "Commands:"
            echo "  cabal build       - Build all solutions"
            echo "  cabal run aoc22   - Run all 15 days"
            echo "  ormolu -i *.hs    - Format code"
            echo "  hlint .           - Lint code"
            echo ""
            echo "Nix commands:"
            echo "  nix build         - Build package"
            echo "  nix run           - Run all solutions"
          '';
        };
      }
    );
}
