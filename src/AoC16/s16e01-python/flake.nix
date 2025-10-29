{
  description = "Advent of Code 2016 Day 1 solution in Python";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build the Python package using uv
        package = pkgs.python3Packages.buildPythonApplication {
          pname = "s16e01";
          version = "0.1.0";
          src = ./.;
          format = "pyproject";

          nativeBuildInputs = with pkgs.python3Packages; [
            hatchling
          ];

          propagatedBuildInputs = [ ];

          # Don't check during build, we have a separate check
          doCheck = false;
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
            name = "s16e01-tests";
            src = ./.;

            nativeBuildInputs = [
              pkgs.python3
              pkgs.python3Packages.pytest
              pkgs.python3Packages.hatchling
            ];

            buildPhase = ''
              # Install the package in development mode
              export PYTHONPATH=$src:$PYTHONPATH
              python3 -m pytest tests/ -v
            '';

            installPhase = ''
              mkdir -p $out
              echo "Tests passed" > $out/result
            '';
          };

          # Run linting with ruff
          lint = pkgs.stdenv.mkDerivation {
            name = "s16e01-lint";
            src = ./.;

            nativeBuildInputs = [
              pkgs.python3
              pkgs.ruff
            ];

            buildPhase = ''
              ruff check s16e01/ tests/
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
              export PATH=${pkgs.ruff}/bin:$PATH
              exec ${pkgs.ruff}/bin/ruff format s16e01/ tests/
            '');
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            python3
            python3Packages.pytest
            python3Packages.hatchling
            ruff
            uv
          ];

          shellHook = ''
            echo "🎄 Python environment ready"
            echo ""
            echo "Local dev with uv:"
            echo "  uv run pytest       - Run tests"
            echo "  uv run ruff check   - Lint code"
            echo "  uv run ruff format  - Format code"
            echo ""
            echo "Nix commands:"
            echo "  nix build           - Build package"
            echo "  nix run             - Run verification"
            echo "  nix flake check     - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just check          - Run all checks"
            echo "  just run            - Run verification"
          '';
        };
      }
    );
}
