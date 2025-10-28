{
  description = "Advent of Code 2016 Day 1 solution in Rust";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build the Rust package
        package = pkgs.rustPlatform.buildRustPackage {
          pname = "s16e01";
          version = "0.1.0";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;
        };
      in
      {
        packages = {
          default = package;
        };

        checks = {
          # Build succeeds = package is valid
          build = package;

          # Run tests with proper Rust setup
          test = pkgs.rustPlatform.buildRustPackage {
            pname = "s16e01-tests";
            version = "0.1.0";
            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;
            buildPhase = ''
              cargo test --release
            '';
            installPhase = ''
              mkdir -p $out
              echo "Tests passed" > $out/result
            '';
          };

          # Run clippy with proper Rust setup
          lint = pkgs.rustPlatform.buildRustPackage {
            pname = "s16e01-lint";
            version = "0.1.0";
            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;
            nativeBuildInputs = [ pkgs.clippy ];
            buildPhase = ''
              cargo clippy --release -- -D warnings
            '';
            installPhase = ''
              mkdir -p $out
              echo "Lint passed" > $out/result
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
              export PATH=${pkgs.rustfmt}/bin:$PATH
              exec ${pkgs.rustfmt}/bin/cargo-fmt
            '');
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            rustc
            cargo
            rustfmt
            clippy
            rust-analyzer
          ];

          shellHook = ''
            echo "🎄 Rust environment ready"
            echo ""
            echo "Local dev:"
            echo "  cargo build    - Build locally"
            echo "  cargo test     - Run tests"
            echo "  cargo clippy   - Lint code"
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
