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
        nodejs = pkgs.nodejs_20;

        # Build the TypeScript package
        package = pkgs.buildNpmPackage {
          pname = "aoc-solution";
          version = "0.1.0";
          src = ./.;

          # Hash of npm dependencies (locked for reproducible builds)
          npmDepsHash = "sha256-4pB1PLqxg35/7tnh25QtXIZUloWlew5Jq/LtUzCYGZ8=";

          buildPhase = ''
            runHook preBuild

            # Compile TypeScript
            npx tsc --outDir dist

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out/bin $out/lib
            cp -r dist/* $out/lib/

            # Create wrapper scripts
            cat > $out/bin/aoc-solution <<EOF
            #!/bin/sh
            exec ${nodejs}/bin/node $out/lib/main.js "\$@"
            EOF

            cat > $out/bin/part1 <<EOF
            #!/bin/sh
            exec ${nodejs}/bin/node $out/lib/part1.js "\$@"
            EOF

            cat > $out/bin/part2 <<EOF
            #!/bin/sh
            exec ${nodejs}/bin/node $out/lib/part2.js "\$@"
            EOF

            chmod +x $out/bin/aoc-solution $out/bin/part1 $out/bin/part2

            runHook postInstall
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
        };

        apps = {
          # Default: run main verification binary
          default = {
            type = "app";
            program = "${package}/bin/aoc-solution";
          };

          # Run individual parts
          part1 = {
            type = "app";
            program = "${package}/bin/part1";
          };

          part2 = {
            type = "app";
            program = "${package}/bin/part2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs_20
            typescript
            nodePackages.ts-node
          ];

          shellHook = ''
            echo "🎄 TypeScript environment ready"
            echo ""
            echo "Local dev:"
            echo "  npm install          - Install dependencies"
            echo "  tsc                  - Compile TypeScript"
            echo "  ts-node part1.ts     - Run part 1"
            echo "  ts-node part2.ts     - Run part 2"
            echo ""
            echo "Nix commands:"
            echo "  nix build       - Build package"
            echo "  nix run         - Run verification"
            echo "  nix flake check - Run all checks"
            echo ""
            echo "Just shortcuts:"
            echo "  just build       - Build package"
            echo "  just run         - Run verification"
            echo "  just run-part 1  - Run part 1"
            echo "  just check       - Run all checks"
          '';
        };
      }
    );
}
