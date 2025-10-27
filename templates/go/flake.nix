{
  description = "Advent of Code solution in Go";

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
          default = pkgs.buildGoModule {
            pname = "aoc-solution";
            version = "0.1.0";
            src = ./.;

            vendorHash = null; # No external dependencies

            subPackages = [ "cmd/part1" "cmd/part2" ];
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            go
            gopls
          ];

          shellHook = ''
            echo "Go environment ready"
          '';
        };
      }
    );
}
