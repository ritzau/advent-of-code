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

        # Build with kotlinc (Gradle available for local dev)
        package = pkgs.stdenv.mkDerivation {
          pname = "aoc-solution";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = with pkgs; [
            kotlin
            jdk17
          ];

          buildPhase = ''
            # Compile all Kotlin source files with kotlinc
            kotlinc -include-runtime -d aoc-solution.jar \
              src/main/kotlin/common.kt \
              src/main/kotlin/main.kt

            kotlinc -include-runtime -d part1.jar \
              src/main/kotlin/common.kt \
              src/main/kotlin/part1.kt

            kotlinc -include-runtime -d part2.jar \
              src/main/kotlin/common.kt \
              src/main/kotlin/part2.kt
          '';

          installPhase = ''
            mkdir -p $out/bin

            # Install main verification binary
            install -Dm644 aoc-solution.jar $out/share/aoc-solution.jar
            cat > $out/bin/aoc-solution << EOF
            #!/bin/sh
            exec ${pkgs.jdk17}/bin/java -jar $out/share/aoc-solution.jar "\$@"
            EOF
            chmod +x $out/bin/aoc-solution

            # Install part1 binary
            install -Dm644 part1.jar $out/share/part1.jar
            cat > $out/bin/part1 << EOF
            #!/bin/sh
            exec ${pkgs.jdk17}/bin/java -jar $out/share/part1.jar "\$@"
            EOF
            chmod +x $out/bin/part1

            # Install part2 binary
            install -Dm644 part2.jar $out/share/part2.jar
            cat > $out/bin/part2 << EOF
            #!/bin/sh
            exec ${pkgs.jdk17}/bin/java -jar $out/share/part2.jar "\$@"
            EOF
            chmod +x $out/bin/part2
          '';
        };
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

          # Format code
          format = {
            type = "app";
            program = toString (pkgs.writeShellScript "format" ''
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
            echo "Local dev (Gradle):"
            echo "  gradle build           - Build project"
            echo "  gradle test            - Run tests"
            echo "  gradle run             - Run verification"
            echo "  ktlint -F src/**/*.kt  - Format code"
            echo ""
            echo "Nix commands:"
            echo "  nix build              - Build package (with kotlinc)"
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
