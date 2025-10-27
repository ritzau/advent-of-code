{
  description = "Advent of Code solution in Kotlin";

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
          default = pkgs.buildBazelPackage {
            pname = "aoc-solution";
            version = "0.1.0";
            src = ./.;

            bazel = pkgs.bazel_7;

            bazelTargets = [ "//:part1" "//:part2" ];

            fetchAttrs = {
              sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
            };

            buildAttrs = {
              installPhase = ''
                mkdir -p $out/bin
                cp bazel-bin/part1 $out/bin/
                cp bazel-bin/part2 $out/bin/
              '';
            };
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            bazel_7
            kotlin
            kotlin-language-server
            ktlint
          ];

          shellHook = ''
            echo "Kotlin + Bazel environment ready"
          '';
        };
      }
    );
}
