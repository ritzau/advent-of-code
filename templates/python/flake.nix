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
      in
      {
        packages = {
          default = pkgs.stdenv.mkDerivation {
            pname = "aoc-solution";
            version = "0.1.0";
            src = ./.;

            buildInputs = [ pkgs.python3 ];

            buildPhase = ''
              # No compilation needed for Python
            '';

            installPhase = ''
              mkdir -p $out/bin $out/lib
              cp *.py $out/lib/

              # Create wrapper scripts
              cat > $out/bin/part1 <<EOF
              #!/bin/sh
              exec ${pkgs.python3}/bin/python3 $out/lib/part1.py "\$@"
              EOF

              cat > $out/bin/part2 <<EOF
              #!/bin/sh
              exec ${pkgs.python3}/bin/python3 $out/lib/part2.py "\$@"
              EOF

              chmod +x $out/bin/part1 $out/bin/part2
            '';
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            python3
            python3Packages.pytest
            python3Packages.black
          ];

          shellHook = ''
            echo "Python environment ready"
          '';
        };
      }
    );
}
