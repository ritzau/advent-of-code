{
  description = "Advent of Code solution in Julia";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Julia environment with packages
        julia-env = pkgs.julia.withPackages [];

        # Build wrapper scripts for each part
        buildJuliaScript = name: script: pkgs.writeScriptBin name ''
          #!${pkgs.bash}/bin/bash
          exec ${julia-env}/bin/julia ${script} "$@"
        '';

        # Package containing all binaries
        package = pkgs.stdenv.mkDerivation {
          pname = "template-julia";
          version = "0.1.0";
          src = ./.;

          buildInputs = [ julia-env ];

          buildPhase = ''
            # No build phase needed for simple Julia scripts
          '';

          installPhase = ''
            mkdir -p $out/bin $out/src
            cp -r src/* $out/src/

            # Create wrapper for main
            cat > $out/bin/template-julia <<EOF
            #!${pkgs.bash}/bin/bash
            exec ${julia-env}/bin/julia $out/src/main.jl "\$@"
            EOF
            chmod +x $out/bin/template-julia

            # Create wrapper for part1
            cat > $out/bin/template-julia-part1 <<EOF
            #!${pkgs.bash}/bin/bash
            exec ${julia-env}/bin/julia $out/src/part1.jl "\$@"
            EOF
            chmod +x $out/bin/template-julia-part1

            # Create wrapper for part2
            cat > $out/bin/template-julia-part2 <<EOF
            #!${pkgs.bash}/bin/bash
            exec ${julia-env}/bin/julia $out/src/part2.jl "\$@"
            EOF
            chmod +x $out/bin/template-julia-part2
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

          # Run tests
          test = pkgs.stdenv.mkDerivation {
            name = "template-julia-tests";
            src = ./.;
            buildInputs = [ julia-env ];
            buildPhase = ''
              export HOME=$TMPDIR
              ${julia-env}/bin/julia -e 'using Test; include("src/lib.jl"); include("test/runtests.jl")'
            '';
            installPhase = ''
              mkdir -p $out
              echo "Tests passed" > $out/result
            '';
          };

          # Check formatting
          format-check = pkgs.stdenv.mkDerivation {
            name = "template-julia-format-check";
            src = ./.;
            buildInputs = [ julia-env ];
            buildPhase = ''
              # Julia doesn't have a standard formatter in the stdlib yet
              # JuliaFormatter.jl would be needed, but for template we skip this
              echo "Format check not yet implemented for Julia"
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
            program = "${package}/bin/template-julia";
            meta.description = "template-julia: Run all parts";
          };

          # Run individual parts
          template-julia-part1 = {
            type = "app";
            program = "${package}/bin/template-julia-part1";
            meta.description = "template-julia: Run part 1";
          };

          template-julia-part2 = {
            type = "app";
            program = "${package}/bin/template-julia-part2";
            meta.description = "template-julia: Run part 2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Julia environment
            julia-env

            # Common utilities
            just      # Command runner
            jq        # JSON processing
            ripgrep   # Fast search
          ];

          shellHook = ''
            # Mark that we're in this project's Nix shell
            export AOC_NIX_SHELL_ROOT="$PWD"

            echo "🎄 Julia environment ready"
            echo ""
            echo "Available commands (auto-detected):"
            echo "  just build        - Build solution"
            echo "  just run [PART]   - Run verification (part1, part2, or default)"
            echo "  just check-test   - Run tests"
            echo "  just check-format - Check formatting"
            echo "  just format       - Format code (not yet standardized)"
            echo "  just check-all    - Run all checks (hermetic)"
            echo ""
            echo "Environment:"
            echo "  In shell: uses local julia commands (fast)"
            echo "  Outside:  uses nix commands (hermetic)"
            echo "  Set JUST_FORCE_NIX=1 to force Nix mode"
          '';
        };
      }
    );
}
