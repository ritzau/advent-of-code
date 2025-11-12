{
  description = "Advent of Code solution in Go";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build the Go package
        package = pkgs.buildGoModule {
          pname = "template-go";
          version = "0.1.0";
          src = ./.;

          vendorHash = null; # No external dependencies

          subPackages = [ "." "cmd/template-go-part1" "cmd/template-go-part2" ];
        };
      in
      {
        packages = {
          default = package;
        };

        checks = {
          # Build succeeds = package is valid
          build = package;

          # Run tests with proper Go setup
          test = pkgs.stdenv.mkDerivation {
            name = "aoc-solution-tests";
            src = ./.;
            buildInputs = [ pkgs.go ];
            buildPhase = ''
              export HOME=$TMPDIR
              go test ./...
            '';
            installPhase = ''
              mkdir -p $out
              echo "Tests passed" > $out/result
            '';
          };

          # Verify formatting is correct
          format-check = pkgs.stdenv.mkDerivation {
            name = "aoc-solution-format-check";
            src = ./.;
            buildInputs = [ pkgs.go ];
            buildPhase = ''
              # gofmt returns non-zero if files need formatting
              unformatted=$(gofmt -l .)
              if [ -n "$unformatted" ]; then
                echo "The following files need formatting:"
                echo "$unformatted"
                echo "Run 'just format' to fix."
                exit 1
              fi
            '';
            installPhase = ''
              mkdir -p $out
              echo "Format check passed" > $out/result
            '';
          };
        };

        apps = {
          # Default: run main verification binary
          default = {
            type = "app";
            program = "${package}/bin/template-go";
            meta.description = "template-go: Run all parts";
          };

          # Run individual parts
          template-go-part1 = {
            type = "app";
            program = "${package}/bin/template-go-part1";
            meta.description = "template-go: Run part 1";
          };

          template-go-part2 = {
            type = "app";
            program = "${package}/bin/template-go-part2";
            meta.description = "template-go: Run part 2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Go toolchain
            go
            gopls

            # Common utilities
            just      # Command runner
            jq        # JSON processing
            ripgrep   # Fast search
          ];

          shellHook = ''
            # Mark that we're in this project's Nix shell
            export AOC_NIX_SHELL_ROOT="$PWD"

            echo "🎄 Go environment ready"
            echo ""
            echo "Available commands (auto-detected):"
            echo "  just build        - Build solution"
            echo "  just run [PART]   - Run verification (part1, part2, or default)"
            echo "  just check-test   - Run tests"
            echo "  just check-format - Check formatting"
            echo "  just format       - Format code"
            echo "  just check-all    - Run all checks (hermetic)"
            echo ""
            echo "Environment:"
            echo "  In shell: uses local go commands (fast)"
            echo "  Outside:  uses nix commands (hermetic)"
            echo "  Set JUST_FORCE_NIX=1 to force Nix mode"
          '';
        };
      }
    );
}
