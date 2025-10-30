{
  description = "Advent of Code 2016 Day 1 solution in Kotlin (Bazel)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build with kotlinc directly for Nix
        # (Bazel files are available for local development)
        package = pkgs.stdenv.mkDerivation {
          pname = "s16e01-kotlin";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = with pkgs; [
            kotlin
            jdk17
          ];

          buildPhase = ''
            # Compile all Kotlin source files
            kotlinc -include-runtime -d s16e01.jar \
              src/main/kotlin/Common.kt \
              src/main/kotlin/Main.kt

            kotlinc -include-runtime -d part1.jar \
              src/main/kotlin/Common.kt \
              src/main/kotlin/Part1.kt

            kotlinc -include-runtime -d part2.jar \
              src/main/kotlin/Common.kt \
              src/main/kotlin/Part2.kt
          '';

          installPhase = ''
            mkdir -p $out/bin

            # Install main verification binary
            install -Dm644 s16e01.jar $out/share/s16e01.jar
            cat > $out/bin/s16e01 << EOF
            #!/bin/sh
            exec ${pkgs.jdk17}/bin/java -jar $out/share/s16e01.jar "\$@"
            EOF
            chmod +x $out/bin/s16e01

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

          # Note: Bazel tests require network access for rules_kotlin dependencies
          # Use 'bazel test //:test' locally in dev shell for testing
        };

        apps = {
          # Default: run main verification binary
          default = {
            type = "app";
            program = "${package}/bin/s16e01";
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
            bazel_7
            kotlin
            kotlin-language-server
            ktlint
          ];

          shellHook = ''
            echo "🎄 Kotlin + Bazel environment ready"
            echo ""
            echo "Local dev:"
            echo "  bazel build //:s16e01 //:part1 //:part2  - Build all targets"
            echo "  bazel test //:test                        - Run tests"
            echo "  bazel run //:s16e01                       - Run verification"
            echo "  ktlint -F src/**/*.kt                     - Format code"
            echo ""
            echo "Nix commands:"
            echo "  nix build                                 - Build package"
            echo "  nix run                                   - Run verification"
            echo "  nix flake check                           - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just check                                - Run all checks"
            echo "  just run                                  - Run verification"
          '';
        };
      }
    );
}
