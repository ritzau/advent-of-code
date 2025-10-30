{
  description = "Advent of Code solution in Kotlin (Bazel)";

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
          pname = "aoc-solution";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = with pkgs; [
            kotlin
            jdk17
          ];

          buildPhase = ''
            # Compile all Kotlin source files
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

        # For local development without building the full package
        devBuild = pkgs.writeShellScriptBin "dev-build" ''
          ${pkgs.bazel_7}/bin/bazel build //:main //:part1 //:part2
        '';

        devTest = pkgs.writeShellScriptBin "dev-test" ''
          ${pkgs.bazel_7}/bin/bazel test //:test
        '';
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
            bazel_7
            kotlin
            kotlin-language-server
            ktlint
          ];

          shellHook = ''
            echo "🎄 Kotlin + Bazel environment ready"
            echo ""
            echo "Local dev:"
            echo "  bazel build //:main //:part1 //:part2  - Build all targets"
            echo "  bazel test //:test                     - Run tests"
            echo "  bazel run //:main                      - Run verification"
            echo "  ktlint -F src/**/*.kt                  - Format code"
            echo ""
            echo "Nix commands:"
            echo "  nix build                              - Build package"
            echo "  nix run                                - Run verification"
            echo "  nix flake check                        - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just check                             - Run all checks"
            echo "  just run                               - Run verification"
          '';
        };
      }
    );
}
