{
  description = "Advent of Code 2016 Day 1 solution in Go";

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
          pname = "s16e01";
          version = "0.1.0";
          src = ./.;

          vendorHash = null; # No external dependencies

          subPackages = [ "." "cmd/part1" "cmd/part2" ];
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
            name = "s16e01-tests";
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
              export PATH=${pkgs.go}/bin:$PATH
              exec ${pkgs.go}/bin/gofmt -w .
            '');
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            go
            gopls
          ];

          shellHook = ''
            echo "🎄 Go environment ready"
            echo ""
            echo "Local dev:"
            echo "  go build       - Build locally"
            echo "  go test ./...  - Run tests"
            echo "  go fmt ./...   - Format code"
            echo ""
            echo "Nix commands:"
            echo "  nix build      - Build package"
            echo "  nix run        - Run verification"
            echo "  nix flake check - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just check     - Run all checks"
            echo "  just run       - Run verification"
          '';
        };
      }
    );
}
