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

              # mkYarnPackage creates a specific structure:
              # - libexec/AoC23/deps/AoC23/ contains our source
              # - libexec/AoC23/node_modules/ contains dependencies

              # Create wrapper script that uses the mkYarnPackage structure directly
              cat > $out/bin/aoc23 <<EOF
              #!/bin/sh
              cd $out/libexec/AoC23/deps/AoC23
              exec ${nodejs}/bin/node $out/libexec/AoC23/node_modules/.bin/ts-node index.ts "\$@"
              EOF

              chmod +x $out/bin/aoc23

              runHook postInstall
            '';
          };
        };

        checks = {
          # Build succeeds = package is valid
          build = self.packages.${system}.default;

          # Verify TypeScript compiles without errors
          typecheck = pkgs.stdenv.mkDerivation {
            name = "aoc23-typecheck";
            src = ./.;
            nativeBuildInputs = [ nodejs pkgs.yarn ];
            buildPhase = ''
              export HOME=$TMPDIR
              yarn install --frozen-lockfile
              yarn tsc --noEmit
            '';
            installPhase = ''
              mkdir -p $out
              echo "Type check passed" > $out/result
            '';
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
            echo "  just run-part 1 1    - Run day 1 part 1"
            echo "  just check           - Run all checks"
            echo "  just typecheck       - Type check only"
            echo "  just format-check    - Check formatting"
            echo "  just format          - Format code"
          '';
        };
      }
    );
}
