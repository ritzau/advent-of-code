{
  description = "Advent of Code 2023 - TypeScript Solutions";

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
          default = pkgs.buildNpmPackage {
            pname = "aoc23";
            version = "23.19.0";
            src = ./.;

            npmDepsHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";

            buildPhase = ''
              runHook preBuild
              # No compilation needed - ts-node runs TypeScript directly
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin $out/lib
              cp -r . $out/lib/

              # Create wrapper script to run all solutions
              cat > $out/bin/aoc23 <<EOF
              #!/bin/sh
              cd $out/lib
              exec ${pkgs.nodejs}/bin/node --loader ts-node/esm index.ts "\$@"
              EOF

              chmod +x $out/bin/aoc23

              runHook postInstall
            '';
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs
            typescript
            nodePackages.ts-node
            nodePackages.nodemon
            yarn
          ];

          shellHook = ''
            echo "TypeScript environment ready for AoC 2023"
            echo "Run 'yarn install' to install dependencies"
            echo "Run 'yarn start' to run all solutions"
          '';
        };
      }
    );
}
