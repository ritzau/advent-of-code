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

            # Don't override installPhase - let mkYarnPackage do its thing
            # It will create: libexec/aoc23/node_modules and libexec/aoc23/deps/aoc23/

            postInstall = ''
              # Add our wrapper script after mkYarnPackage's default install
              mkdir -p $out/bin
              cat > $out/bin/aoc23 <<EOF
              #!/bin/sh
              cd $out/libexec/aoc23/deps/aoc23
              exec ${nodejs}/bin/node $out/libexec/aoc23/node_modules/.bin/ts-node index.ts "\$@"
              EOF
              chmod +x $out/bin/aoc23
            '';

            # Skip the dist phase which is causing issues
            doDist = false;
          };
        };

        checks = {
          # Build succeeds = package is valid
          build = self.packages.${system}.default;

          # Verify TypeScript compiles without errors
          typecheck = pkgs.mkYarnPackage {
            name = "aoc23-typecheck";
            src = ./.;

            packageJSON = ./package.json;
            yarnLock = ./yarn.lock;

            buildPhase = ''
              runHook preBuild
              # Run tsc from the installed node_modules
              yarn --offline tsc --noEmit
              runHook postBuild
            '';

            installPhase = ''
              mkdir -p $out
              echo "Type check passed" > $out/result
            '';

            doDist = false;
          };

          # Verify formatting is correct
          format-check = pkgs.stdenv.mkDerivation {
            name = "aoc23-format-check";
            src = ./.;
            nativeBuildInputs = [ pkgs.nodePackages.prettier ];
            buildPhase = ''
              ${pkgs.nodePackages.prettier}/bin/prettier --check "*.ts"
            '';
            installPhase = ''
              mkdir -p $out
              echo "Format check passed" > $out/result
            '';
          };
        };

        apps = {
          # Default: run all solutions
          default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/aoc23";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs_20
            yarn
            nodePackages.ts-node
            nodePackages.prettier
          ];

          shellHook = ''
            echo "🎄 TypeScript environment ready for AoC 2023"
            echo ""
            echo "Local dev:"
            echo "  yarn install         - Install dependencies"
            echo "  yarn start           - Run all solutions"
            echo "  ts-node index.ts 1   - Run day 1"
            echo "  prettier --write .   - Format code"
            echo ""
            echo "Nix commands:"
            echo "  nix build            - Build package"
            echo "  nix run              - Run all solutions"
            echo "  nix run . -- 1       - Run day 1"
            echo "  nix flake check      - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just build           - Build package"
            echo "  just run             - Run all solutions"
            echo "  just run 1           - Run day 1"
            echo "  just check           - Run all checks"
            echo "  just typecheck       - Type check only"
            echo "  just format-check    - Check formatting"
            echo "  just format          - Format code"
          '';
        };
      }
    );
}
