{
  description = "Advent of Code 2016 Day 1 solution in Kotlin";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build the Kotlin package using kotlinc directly
        # (Gradle requires network access for plugins, which doesn't work in Nix sandbox)
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

          # Note: JUnit tests require Gradle with internet access for dependencies
          # Use 'gradle test' locally in dev shell for testing
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
