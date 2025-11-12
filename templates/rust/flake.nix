{
  description = "Advent of Code solution in Rust";

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
          pname = "template-rust";
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
            pname = "template-rust-tests";
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
            pname = "template-rust-lint";
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

          # Verify formatting is correct
          format-check = pkgs.rustPlatform.buildRustPackage {
            pname = "template-rust-format-check";
            version = "0.1.0";
            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;
            nativeBuildInputs = [ pkgs.rustfmt ];
            buildPhase = ''
              cargo fmt --check || (echo "Format check failed. Run 'just format' to fix." && exit 1)
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
            program = "${package}/bin/template-rust";
            meta.description = "template-rust: Run all parts";
          };

          # Run individual parts
          template-rust-part1 = {
            type = "app";
            program = "${package}/bin/template-rust-part1";
            meta.description = "template-rust: Run part 1";
          };

          template-rust-part2 = {
            type = "app";
            program = "${package}/bin/template-rust-part2";
            meta.description = "template-rust: Run part 2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Rust toolchain
            rustc
            cargo
            rustfmt
            clippy
            rust-analyzer

            # Common utilities
            just      # Command runner
            jq        # JSON processing
            ripgrep   # Fast search
          ];

          shellHook = ''
            # Mark that we're in this project's Nix shell
            export AOC_NIX_SHELL_ROOT="$PWD"

            echo "🎄 Rust environment ready"
            echo ""
            echo "Local dev (auto-detected in shell):"
            echo "  just build     - Build locally"
            echo "  just test      - Run tests"
            echo "  just format    - Format code"
            echo ""
            echo "Nix commands:"
            echo "  nix build      - Build package"
            echo "  nix run        - Run verification"
            echo "  nix flake check - Run all checks"
            echo ""
            echo "Environment:"
            echo "  Set JUST_FORCE_NIX=1 to force Nix mode"
          '';
        };
      }
    );
}
