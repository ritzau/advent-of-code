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

        # Build with Bazel
        package = pkgs.buildBazelPackage {
          pname = "s16e01-kotlin";
          version = "0.1.0";
          src = ./.;

          bazel = pkgs.bazel_7;
          bazelTargets = [ "//:s16e01" "//:part1" "//:part2" ];

          removeRulesCC = false;

          fetchAttrs = {
            sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
          };

          buildAttrs = {
            installPhase = ''
              mkdir -p $out/bin

              # Install binaries
              install -Dm755 bazel-bin/s16e01 $out/bin/s16e01
              install -Dm755 bazel-bin/part1 $out/bin/part1
              install -Dm755 bazel-bin/part2 $out/bin/part2
            '';
          };
        };
      in
      {
        packages = {
          default = package;
        };

        checks = {
          # Build succeeds = package is valid
          build = package;

          # Run tests
          test = pkgs.stdenv.mkDerivation {
            name = "s16e01-kotlin-tests";
            src = ./.;
            nativeBuildInputs = [ pkgs.bazel_7 ];
            buildPhase = ''
              export HOME=$TMPDIR
              bazel test //:test
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
