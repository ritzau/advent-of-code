{
  description = "Advent of Code solution in Go";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Build part1 binary
        part1 = pkgs.buildGoModule {
          pname = "aoc-part1";
          version = "0.1.0";
          
          src = ./.;
          
          vendorHash = null; # No external dependencies in template
          
          # Build only the part1 binary
          buildPhase = ''
            runHook preBuild
            go build -o part1 part1.go common.go
            runHook postBuild
          '';
          
          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin
            cp part1 $out/bin/part1
            runHook postInstall
          '';
          
          meta = with pkgs.lib; {
            description = "Advent of Code solution part 1 in Go";
            license = licenses.mit;
          };
        };
        
        # Build part2 binary
        part2 = pkgs.buildGoModule {
          pname = "aoc-part2";
          version = "0.1.0";
          
          src = ./.;
          
          vendorHash = null; # No external dependencies in template
          
          # Build only the part2 binary
          buildPhase = ''
            runHook preBuild
            go build -o part2 part2.go common.go
            runHook postBuild
          '';
          
          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin
            cp part2 $out/bin/part2
            runHook postInstall
          '';
          
          meta = with pkgs.lib; {
            description = "Advent of Code solution part 2 in Go";
            license = licenses.mit;
          };
        };
        
        # Combined package with both binaries
        aoc-solution = pkgs.symlinkJoin {
          name = "aoc-solution";
          paths = [ part1 part2 ];
          meta = with pkgs.lib; {
            description = "Advent of Code solution in Go";
            license = licenses.mit;
          };
        };
      in
      {
        # Package outputs for building
        packages = {
          default = aoc-solution;
          aoc-solution = aoc-solution;
        };
        
        # App outputs for running
        apps = {
          part1 = {
            type = "app";
            program = "${part1}/bin/part1";
          };
          part2 = {
            type = "app";
            program = "${part2}/bin/part2";
          };
        };
        
        # Development shell
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            go
            gopls
            gotools
          ];

          shellHook = ''
            echo "Go environment ready"
          '';
        };
      }
    );
}
