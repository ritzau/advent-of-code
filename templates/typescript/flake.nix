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

        # Hash of npm dependencies (locked for reproducible builds)
        npmDepsHash = "sha256-4pB1PLqxg35/7tnh25QtXIZUloWlew5Jq/LtUzCYGZ8=";

        # Build the TypeScript package
        package = pkgs.buildNpmPackage {
          pname = "template-typescript";
          version = "0.1.0";
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

            # Create wrapper scripts
            cat > $out/bin/template-typescript <<EOF
            #!/bin/sh
            exec ${nodejs}/bin/node $out/lib/main.js "\$@"
            EOF

            cat > $out/bin/template-typescript-part1 <<EOF
            #!/bin/sh
            exec ${nodejs}/bin/node $out/lib/part1.js "\$@"
            EOF

            cat > $out/bin/template-typescript-part2 <<EOF
            #!/bin/sh
            exec ${nodejs}/bin/node $out/lib/part2.js "\$@"
            EOF

            chmod +x $out/bin/template-typescript $out/bin/template-typescript-part1 $out/bin/template-typescript-part2

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

          # Verify TypeScript compiles without errors
          typecheck = pkgs.buildNpmPackage {
            name = "template-typescript-typecheck";
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
            name = "template-typescript-format-check";
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
          # Default: run main verification binary
          default = {
            type = "app";
            program = "${package}/bin/template-typescript";
            meta.description = "template-typescript: Run all parts";
          };

          # Run individual parts
          template-typescript-part1 = {
            type = "app";
            program = "${package}/bin/template-typescript-part1";
            meta.description = "template-typescript: Run part 1";
          };

          template-typescript-part2 = {
            type = "app";
            program = "${package}/bin/template-typescript-part2";
            meta.description = "template-typescript: Run part 2";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # TypeScript toolchain
            nodejs_20
            typescript
            nodePackages.ts-node
            nodePackages.prettier

            # Common utilities
            just      # Command runner
            jq        # JSON processing
            ripgrep   # Fast search
          ];

          shellHook = ''
            # Mark that we're in this project's Nix shell
            export AOC_NIX_SHELL_ROOT="$PWD"

            echo "🎄 TypeScript environment ready"
            echo ""
            echo "Available commands (auto-detected):"
            echo "  just build        - Build solution"
            echo "  just run [PART]   - Run verification (part1, part2, or default)"
            echo "  just check-type   - Type check"
            echo "  just check-format - Check formatting"
            echo "  just format       - Format code"
            echo "  just check-all    - Run all checks (hermetic)"
            echo ""
            echo "Environment:"
            echo "  In shell: uses local tsc/ts-node/prettier (fast)"
            echo "  Outside:  uses nix commands (hermetic)"
            echo "  Set JUST_FORCE_NIX=1 to force Nix mode"
          '';
        };
      }
    );
}
