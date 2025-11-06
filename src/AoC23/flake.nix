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

        # Hash of npm dependencies
        npmDepsHash = "sha256-WnSKoxMFjLztOlHE31xKN3fsa/CzZfqM124tCH7YD2U=";

        # List of all days (01-19)
        days = [ "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" ];

        # Build the TypeScript package
        package = pkgs.buildNpmPackage {
          pname = "aoc23";
          version = "23.19.0";
          src = ./.;

          inherit npmDepsHash;

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

            # Create wrapper scripts for part1 and part2 binaries
            ${pkgs.lib.concatMapStringsSep "\n" (day: ''
              cat > $out/bin/s23e${day}-part1 <<EOF
              #!/bin/sh
              exec ${nodejs}/bin/node $out/lib/s23e${day}/s23e${day}-part1.js "\$@"
              EOF
              chmod +x $out/bin/s23e${day}-part1

              cat > $out/bin/s23e${day}-part2 <<EOF
              #!/bin/sh
              exec ${nodejs}/bin/node $out/lib/s23e${day}/s23e${day}-part2.js "\$@"
              EOF
              chmod +x $out/bin/s23e${day}-part2
            '') days}

            runHook postInstall
          '';
        };
      in
      {
        packages = {
          default = package;
          aoc23 = package;
        };

        checks = {
            # Build succeeds = package is valid
            build = package;

            # Run all vitest tests
            tests = pkgs.buildNpmPackage {
              name = "aoc23-tests";
              src = ./.;

              inherit npmDepsHash;

              buildPhase = ''
                runHook preBuild
                # Run all vitest tests
                npx vitest run
                runHook postBuild
              '';

              installPhase = ''
                mkdir -p $out
                echo "All tests passed" > $out/result
              '';
            };

            # Verify TypeScript compiles without errors
            typecheck = pkgs.buildNpmPackage {
              name = "aoc23-typecheck";
              src = ./.;

              inherit npmDepsHash;

              buildPhase = ''
                runHook preBuild
                npx tsc --noEmit
                runHook postBuild
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
                ${pkgs.nodePackages.prettier}/bin/prettier --check "lib/**/*.ts" "s23e*/**/*.ts"
              '';
              installPhase = ''
                mkdir -p $out
                echo "Format check passed" > $out/result
              '';
            };
          };

        apps =
          let
            # Create apps for each day (part1 and part2)
            dayApps = pkgs.lib.listToAttrs (
              pkgs.lib.flatten (map (day: [
                {
                  name = "s23e${day}-part1";
                  value = {
                    type = "app";
                    program = "${package}/bin/s23e${day}-part1";
                  };
                }
                {
                  name = "s23e${day}-part2";
                  value = {
                    type = "app";
                    program = "${package}/bin/s23e${day}-part2";
                  };
                }
              ]) days)
            );
          in
          dayApps // {
            # Default app runs day 1 part 1
            default = {
              type = "app";
              program = "${package}/bin/s23e01-part1";
            };
          };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs_20
            typescript
            nodePackages.ts-node
            nodePackages.prettier
          ];

          shellHook = ''
            echo "🎄 TypeScript environment ready for AoC 2023"
            echo ""
            echo "Local dev:"
            echo "  npm install                      - Install dependencies"
            echo "  npm test                         - Run all vitest tests"
            echo "  tsc                              - Compile TypeScript"
            echo "  prettier --write .               - Format code"
            echo ""
            echo "Nix commands:"
            echo "  nix build                        - Build all days"
            echo "  nix run .#s23e01-part1 < input   - Run day 1 part 1"
            echo "  nix flake check                  - Run all checks (tests, typecheck, format)"
            echo ""
            echo "Just shortcuts:"
            echo "  just build           - Build package"
            echo "  just check-all       - Run all checks"
            echo "  just typecheck       - Type check only"
            echo "  just format-check    - Check formatting"
            echo "  just format          - Format code"
          '';
        };
      }
    );
}
