{
  description = "Advent of Code solution in Kotlin";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable"; };

  inputs.systems.url = "github:nix-systems/default";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-utils.inputs.systems.follows = "systems";

  outputs = { self, systems, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        updateLocks = pkgs.callPackage ./update-locks.nix { };
        package = pkgs.callPackage ./build.nix {
          jdk = pkgs.temurin-bin-21;
        };
        jdk = pkgs.temurin-bin-21;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.gradle_8
            pkgs.temurin-bin-21
            updateLocks
            pkgs.ktlint
          ];

          shellHook = ''
            echo "🎄 Kotlin environment ready"
            echo ""
            echo "Local dev:"
            echo "  ./gradlew build        - Build with Gradle"
            echo "  ./gradlew run          - Run application"
            echo "  ktlint -F src/**/*.kt  - Format code"
            echo "  update-locks           - Update dependency locks"
            echo ""
            echo "Nix commands:"
            echo "  nix build              - Build package"
            echo "  nix run                - Run verification"
            echo "  nix flake check        - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just check             - Run all checks"
            echo "  just run               - Run verification"
          '';
        };
        packages.default = package;

        apps = {
          default = {
            type = "app";
            program = "${package}/bin/aoc-solution";
          };

          part1 = {
            type = "app";
            program = toString (pkgs.writeShellScript "part1" ''
              exec ${jdk}/bin/java -cp ${package}/lib/'*' Part1Kt
            '');
          };

          part2 = {
            type = "app";
            program = toString (pkgs.writeShellScript "part2" ''
              exec ${jdk}/bin/java -cp ${package}/lib/'*' Part2Kt
            '');
          };

          format = {
            type = "app";
            program = toString (pkgs.writeShellScript "format" ''
              exec ${pkgs.ktlint}/bin/ktlint -F "src/**/*.kt"
            '');
          };
        };
      });
}
