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

        # Build with Bazel
        package = pkgs.buildBazelPackage {
          pname = "aoc-solution";
          version = "0.1.0";
          src = ./.;

          bazel = pkgs.bazel_7;
          bazelTargets = [ "//:main" "//:part1" "//:part2" ];

          removeRulesCC = false;
          removeLocalConfigCc = false;

          fetchAttrs = {
            # TODO: Update this hash by running:
            # nix-build -A aoc-solution 2>&1 | grep "got:" | awk '{print $2}'
            # Or let it fail once and copy the hash from the error message
            sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
          };

          buildAttrs = {
            installPhase = ''
              mkdir -p $out/bin

              # Install binaries (they are wrapper scripts created by kt_jvm_binary)
              install -Dm755 bazel-bin/main $out/bin/aoc-solution
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
