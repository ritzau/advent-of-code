{
  description = "Advent of Code solution in Zig";

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

            nativeBuildInputs = [ pkgs.zig ];

            buildPhase = ''
              zig build-exe part1.zig -O ReleaseSafe -femit-bin=part1
              zig build-exe part2.zig -O ReleaseSafe -femit-bin=part2
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
            zig
            zls
          ];

          shellHook = ''
            echo "Zig environment ready"
          '';
        };
      }
    );
}
