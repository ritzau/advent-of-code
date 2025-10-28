{
  description = "Advent of Code solution in TypeScript";

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

            buildInputs = [ pkgs.nodejs pkgs.typescript ];

            buildPhase = ''
              runHook preBuild

              # Compile TypeScript
              ${pkgs.typescript}/bin/tsc --outDir dist

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin $out/lib
              cp -r dist/* $out/lib/

              # Create wrapper scripts
              cat > $out/bin/part1 <<EOF
              #!/bin/sh
              exec ${pkgs.nodejs}/bin/node $out/lib/part1.js "\$@"
              EOF

              cat > $out/bin/part2 <<EOF
              #!/bin/sh
              exec ${pkgs.nodejs}/bin/node $out/lib/part2.js "\$@"
              EOF

              chmod +x $out/bin/part1 $out/bin/part2

              runHook postInstall
            '';
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs
            typescript
            nodePackages.ts-node
          ];

          shellHook = ''
            echo "TypeScript environment ready"
          '';
        };
      }
    );
}
