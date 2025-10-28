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
        nodejs = pkgs.nodejs_20;
      in
      {
        packages = {
          default = pkgs.mkYarnPackage {
            pname = "aoc23";
            version = "23.19.0";
            src = ./.;
            
            packageJSON = ./package.json;
            yarnLock = ./yarn.lock;

            buildPhase = ''
              runHook preBuild
              # No compilation needed - ts-node runs TypeScript directly
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin
              
              # mkYarnPackage creates deps/aoc23 structure
              # Copy the entire structure to $out/lib
              mkdir -p $out/lib
              cp -r deps/AoC23/* $out/lib/ 2>/dev/null || true
              cp -r node_modules $out/lib/

              # Create wrapper script to run all solutions
              cat > $out/bin/aoc23 <<EOF
              #!/bin/sh
              cd $out/lib
              exec ${nodejs}/bin/node node_modules/.bin/ts-node index.ts "\$@"
              EOF

              chmod +x $out/bin/aoc23

              runHook postInstall
            '';
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs_20
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
