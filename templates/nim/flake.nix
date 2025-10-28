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
      in
      {
        packages = {
          default = pkgs.buildNimPackage (finalAttrs: {
            pname = "aoc-solution";
            version = "0.1.0";
            src = ./.;

            lockFile = ./lock.json;

            nimbleFile = ./aoc_solution.nimble;

            nimFlags = [ "-d:NimblePkgVersion=${finalAttrs.version}" ];
          });
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nim
            nimlsp
          ];

          shellHook = ''
            echo "Nim environment ready"
          '';
        };
      }
    );
}
