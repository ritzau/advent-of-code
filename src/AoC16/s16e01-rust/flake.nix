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
          # Run tests as a check
          test = pkgs.stdenv.mkDerivation {
            name = "s16e01-tests";
            src = ./.;
            buildInputs = [ pkgs.cargo pkgs.rustc ];
            buildPhase = ''
              cargo test
            '';
            installPhase = ''
              mkdir -p $out
              echo "Tests passed" > $out/result
            '';
          };

          # Run clippy as a check
          lint = pkgs.stdenv.mkDerivation {
            name = "s16e01-lint";
            src = ./.;
            buildInputs = [ pkgs.cargo pkgs.rustc pkgs.clippy ];
            buildPhase = ''
              cargo clippy -- -D warnings
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

          # Run tests
          test = {
            type = "app";
            program = toString (pkgs.writeShellScript "test" ''
              export PATH=${pkgs.cargo}/bin:$PATH
              exec ${pkgs.cargo}/bin/cargo test
            '');
          };

          # Run clippy linter
          lint = {
            type = "app";
            program = toString (pkgs.writeShellScript "lint" ''
              export PATH=${pkgs.cargo}/bin:$PATH
              exec ${pkgs.cargo}/bin/cargo clippy -- -D warnings
            '');
          };

          # Format code
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
            echo "  cargo build   - Build locally"
            echo "  cargo test    - Run tests locally"
            echo "  nix build     - Build with Nix"
            echo "  nix run       - Run main verification"
          '';
        };
      }
    );
}
