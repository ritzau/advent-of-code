{
  description = "Advent of Code solution in Python";

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
          pname = "template-python";
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
            name = "template-python-tests";
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
            name = "template-python-lint";
            src = ./.;

            nativeBuildInputs = [
              pkgs.python3
              pkgs.ruff
            ];

            buildPhase = ''
              ruff check template_python/ tests/
            '';

            installPhase = ''
              mkdir -p $out
              echo "Lint passed" > $out/result
            '';
          };

          # Verify formatting is correct
          format-check = pkgs.stdenv.mkDerivation {
            name = "template-python-format-check";
            src = ./.;

            nativeBuildInputs = [
              pkgs.python3
              pkgs.ruff
            ];

            buildPhase = ''
              ruff format --check template_python/ tests/ || (echo "Format check failed. Run 'just format' to fix." && exit 1)
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
            program = "${package}/bin/template-python";
            meta.description = "template-python: Run all parts";
          };

          # Run individual parts
          template-python-part1 = {
            type = "app";
            program = "${package}/bin/template-python-part1";
            meta.description = "template-python: Run part 1";
          };

          template-python-part2 = {
            type = "app";
            program = "${package}/bin/template-python-part2";
            meta.description = "template-python: Run part 2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Python toolchain
            python3
            python3Packages.pytest
            python3Packages.hatchling
            ruff
            uv

            # Common utilities
            just      # Command runner
            jq        # JSON processing
            ripgrep   # Fast search
          ];

          shellHook = ''
            # Mark that we're in this project's Nix shell
            export AOC_NIX_SHELL_ROOT="$PWD"

            echo "🎄 Python environment ready"
            echo ""
            echo "Available commands (auto-detected):"
            echo "  just build        - Build solution"
            echo "  just run [PART]   - Run verification (part1, part2, or default)"
            echo "  just check-test   - Run tests"
            echo "  just check-lint   - Run linter"
            echo "  just check-format - Check formatting"
            echo "  just format       - Format code"
            echo "  just check-all    - Run all checks (hermetic)"
            echo ""
            echo "Environment:"
            echo "  In shell: uses local uv/ruff commands (fast)"
            echo "  Outside:  uses nix commands (hermetic)"
            echo "  Set JUST_FORCE_NIX=1 to force Nix mode"
          '';
        };
      }
    );
}
