{
  description = "Advent of Code C++ Template";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        template-cpp = pkgs.stdenv.mkDerivation {
          pname = "template-cpp";
          version = "1.0.0";

          src = ./.;

          nativeBuildInputs = [ pkgs.cmake ];

          buildPhase = ''
            cmake -DCMAKE_BUILD_TYPE=Release .
            make -j$NIX_BUILD_CORES
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp template-cpp $out/bin/
            cp template-cpp-part1 $out/bin/
            cp template-cpp-part2 $out/bin/
          '';

          meta = {
            description = "Advent of Code C++ template solution";
            license = pkgs.lib.licenses.mit;
          };
        };

      in
      {
        packages.default = template-cpp;

        checks = {
          build = template-cpp;

          test = pkgs.stdenv.mkDerivation {
            name = "template-cpp-test";
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
            name = "template-cpp-format-check";
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
            program = "${template-cpp}/bin/template-cpp";
          };

          template-cpp-part1 = {
            type = "app";
            program = "${template-cpp}/bin/template-cpp-part1";
          };

          template-cpp-part2 = {
            type = "app";
            program = "${template-cpp}/bin/template-cpp-part2";
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
