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

        # Build the Kotlin package using Gradle
        package = pkgs.stdenv.mkDerivation (finalAttrs: {
          pname = "aoc-solution";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = with pkgs; [
            gradle
            jdk17
            makeWrapper
          ];

          # Gradle dependencies cache
          mitmCache = pkgs.gradle.fetchDeps {
            pkg = finalAttrs.finalPackage;
            data = ./deps.json;
          };

          # Required for mitm-cache on Darwin
          __darwinAllowLocalNetworking = true;

          gradleBuildTask = "buildAll";

          installPhase = ''
            mkdir -p $out/bin

            # Install main verification binary
            install -Dm644 build/libs/aoc-solution-*.jar $out/share/aoc-solution.jar
            makeWrapper ${pkgs.jdk17}/bin/java $out/bin/aoc-solution \
              --add-flags "-jar $out/share/aoc-solution.jar"

            # Install part1 binary
            install -Dm644 build/libs/part1-*.jar $out/share/part1.jar
            makeWrapper ${pkgs.jdk17}/bin/java $out/bin/part1 \
              --add-flags "-jar $out/share/part1.jar"

            # Install part2 binary
            install -Dm644 build/libs/part2-*.jar $out/share/part2.jar
            makeWrapper ${pkgs.jdk17}/bin/java $out/bin/part2 \
              --add-flags "-jar $out/share/part2.jar"
          '';

          meta.sourceProvenance = with pkgs.lib.sourceTypes; [
            fromSource
            binaryBytecode  # mitm cache
          ];
        });
      in
      {
        packages = {
          default = package;
        };

        checks = {
          # Build succeeds = package is valid
          build = package;
        };

        apps = {
          # Default: run main verification binary
          default = {
            type = "app";
            program = "${package}/bin/aoc-solution";
          };

          # Run individual parts
          part1 = {
            type = "app";
            program = "${package}/bin/part1";
          };

          part2 = {
            type = "app";
            program = "${package}/bin/part2";
          };

          # Format code (app because it modifies files)
          format = {
            type = "app";
            program = toString (pkgs.writeShellScript "format" ''
              export PATH=${pkgs.ktlint}/bin:$PATH
              exec ${pkgs.ktlint}/bin/ktlint -F "src/**/*.kt"
            '');
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            gradle
            jdk17
            kotlin
            kotlin-language-server
            ktlint
          ];

          shellHook = ''
            echo "🎄 Kotlin environment ready"
            echo ""
            echo "Local dev:"
            echo "  gradle build           - Build project"
            echo "  gradle test            - Run tests"
            echo "  gradle run             - Run verification"
            echo "  ktlint -F src/**/*.kt  - Format code"
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
      }
    );
}
