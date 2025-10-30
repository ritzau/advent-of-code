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
        packages.default = pkgs.callPackage ./build.nix {
          jdk = pkgs.temurin-bin-21;
        };
      });
}
