{
  description = "Advent of Code 2016 Day 1 solution in C++";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        package = pkgs.stdenv.mkDerivation {
          pname = "s16e01-cpp";
          version = "1.0.0";

          src = ./.;

          nativeBuildInputs = [ pkgs.cmake ];

          buildPhase = ''
            cmake -DCMAKE_BUILD_TYPE=Release .
            make -j$NIX_BUILD_CORES
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp s16e01-cpp $out/bin/
            cp s16e01-cpp-part1 $out/bin/
            cp s16e01-cpp-part2 $out/bin/
          '';

          meta = {
            description = "Advent of Code 2016 Day 1 solution in C++";
            license = pkgs.lib.licenses.mit;
          };
        };

      in
      {
        packages.default = package;

        checks = {
          build = package;

          test = pkgs.stdenv.mkDerivation {
            name = "s16e01-cpp-test";
            src = ./.;

            nativeBuildInputs = [ pkgs.cmake ];

            buildPhase = ''
              cmake -DCMAKE_BUILD_TYPE=Release .
              make -j$NIX_BUILD_CORES
            '';

            checkPhase = ''
              ctest --output-on-failure
            '';

            installPhase = ''
              mkdir -p $out
              echo "Tests passed" > $out/test-results
            '';

            doCheck = true;
          };

          format-check = pkgs.stdenv.mkDerivation {
            name = "s16e01-cpp-format-check";
            src = ./.;

            nativeBuildInputs = [ pkgs.clang-tools ];

            buildPhase = ''
              # Check if any files would be reformatted
              if ! find src tests -name "*.cpp" -o -name "*.h" | xargs clang-format --dry-run -Werror 2>&1; then
                echo "Format check failed. Run 'just format' to fix."
                exit 1
              fi
            '';

            installPhase = ''
              mkdir -p $out
              echo "Format check passed" > $out/format-results
            '';
          };
        };

        apps = {
          default = {
            type = "app";
            program = "${package}/bin/s16e01-cpp";
            meta.description = "s16e01-cpp: Run all parts";
          };

          s16e01-cpp = {
            type = "app";
            program = "${package}/bin/s16e01-cpp";
            meta.description = "s16e01-cpp: Run all parts";
          };

          s16e01-cpp-part1 = {
            type = "app";
            program = "${package}/bin/s16e01-cpp-part1";
            meta.description = "s16e01-cpp: Run part 1";
          };

          s16e01-cpp-part2 = {
            type = "app";
            program = "${package}/bin/s16e01-cpp-part2";
            meta.description = "s16e01-cpp: Run part 2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.cmake
            pkgs.clang-tools
            pkgs.just
          ];
        };
      }
    );
}
