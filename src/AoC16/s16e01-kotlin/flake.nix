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

        # Build the Kotlin package using Gradle
        package = pkgs.stdenv.mkDerivation {
          pname = "s16e01-kotlin";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = with pkgs; [
            gradle
            jdk17
          ];

          buildPhase = ''
            export GRADLE_USER_HOME=$TMPDIR/gradle
            gradle --no-daemon buildAll
          '';

          installPhase = ''
            mkdir -p $out/bin

            # Install main verification binary
            cp build/libs/s16e01-*.jar $out/bin/s16e01.jar
            cat > $out/bin/s16e01 << 'EOF'
            #!/bin/sh
            exec ${pkgs.jdk17}/bin/java -jar $out/bin/s16e01.jar "$@"
            EOF
            chmod +x $out/bin/s16e01

            # Install part1 binary
            cp build/libs/part1-*.jar $out/bin/part1.jar
            cat > $out/bin/part1 << 'EOF'
            #!/bin/sh
            exec ${pkgs.jdk17}/bin/java -jar $out/bin/part1.jar "$@"
            EOF
            chmod +x $out/bin/part1

            # Install part2 binary
            cp build/libs/part2-*.jar $out/bin/part2.jar
            cat > $out/bin/part2 << 'EOF'
            #!/bin/sh
            exec ${pkgs.jdk17}/bin/java -jar $out/bin/part2.jar "$@"
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

          # Run tests with Gradle
          test = pkgs.stdenv.mkDerivation {
            name = "s16e01-kotlin-tests";
            src = ./.;
            nativeBuildInputs = with pkgs; [ gradle jdk17 ];
            buildPhase = ''
              export GRADLE_USER_HOME=$TMPDIR/gradle
              gradle --no-daemon test
            '';
            installPhase = ''
              mkdir -p $out
              echo "Tests passed" > $out/result
            '';
          };
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
