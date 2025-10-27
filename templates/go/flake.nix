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
          default = pkgs.stdenv.mkDerivation {
            pname = "aoc-solution";
            version = "0.1.0";
            src = ./.;

            nativeBuildInputs = [ pkgs.go ];

            buildPhase = ''
              export HOME=$TMPDIR
              go build -o part1 part1.go common.go
              go build -o part2 part2.go common.go
            '';

            installPhase = ''
              mkdir -p $out/bin
              cp part1 $out/bin/
              cp part2 $out/bin/
            '';
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
