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
          default = pkgs.stdenv.mkDerivation {
            pname = "aoc-solution";
            version = "0.1.0";
            src = ./.;

            nativeBuildInputs = [ pkgs.nim ];

            buildPhase = ''
              runHook preBuild

              nim c -d:release --hints:off --verbosity:0 --out:part1 part1.nim
              nim c -d:release --hints:off --verbosity:0 --out:part2 part2.nim

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin
              cp part1 $out/bin/
              cp part2 $out/bin/

              runHook postInstall
            '';
          };
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
