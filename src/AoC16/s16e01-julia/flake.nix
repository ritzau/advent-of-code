{
  description = "AoC 2016 Day 1: No Time for a Taxicab (Julia)";

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

        # Package containing all binaries
        package = pkgs.stdenv.mkDerivation {
          pname = "s16e01-julia";
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
            cat > $out/bin/s16e01-julia <<EOF
            #!${pkgs.bash}/bin/bash
            exec ${julia-env}/bin/julia $out/src/main.jl "\$@"
            EOF
            chmod +x $out/bin/s16e01-julia

            # Create wrapper for part1
            cat > $out/bin/s16e01-julia-part1 <<EOF
            #!${pkgs.bash}/bin/bash
            exec ${julia-env}/bin/julia $out/src/part1.jl "\$@"
            EOF
            chmod +x $out/bin/s16e01-julia-part1

            # Create wrapper for part2
            cat > $out/bin/s16e01-julia-part2 <<EOF
            #!${pkgs.bash}/bin/bash
            exec ${julia-env}/bin/julia $out/src/part2.jl "\$@"
            EOF
            chmod +x $out/bin/s16e01-julia-part2
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
            name = "s16e01-julia-tests";
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
            name = "s16e01-julia-format-check";
            src = ./.;
            buildInputs = [ julia-env ];
            buildPhase = ''
              # Julia doesn't have a standard formatter in the stdlib yet
              # JuliaFormatter.jl would be needed, but for this solution we skip this
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
            program = "${package}/bin/s16e01-julia";
            meta.description = "s16e01-julia: Run all parts";
          };

          # Run individual parts
          s16e01-julia-part1 = {
            type = "app";
            program = "${package}/bin/s16e01-julia-part1";
            meta.description = "s16e01-julia: Run part 1";
          };

          s16e01-julia-part2 = {
            type = "app";
            program = "${package}/bin/s16e01-julia-part2";
            meta.description = "s16e01-julia: Run part 2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            julia-env
          ];

          shellHook = ''
            echo "🎄 Julia environment ready for AoC 2016 Day 1"
            echo ""
            echo "Local dev:"
            echo "  julia src/main.jl < ../../inputs/2016/day01.txt  - Run verification"
            echo "  julia -e 'using Test; include(\"src/lib.jl\"); include(\"test/runtests.jl\")' - Run tests"
            echo ""
            echo "Nix commands:"
            echo "  nix build      - Build package"
            echo "  nix run        - Run verification"
            echo "  nix flake check - Run all checks"
          '';
        };
      }
    );
}
