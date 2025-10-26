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
        
        # Build the Go package using buildGoModule
        aoc-solution = pkgs.buildGoModule {
          pname = "aoc-solution";
          version = "0.1.0";
          
          src = ./.;
          
          # vendorHash needs to be set for Go modules
          # Use "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" as placeholder
          # or lib.fakeHash, then update with the correct hash after first build
          vendorHash = null; # No external dependencies in template
          
          # Build both binaries
          subPackages = [ "." ];
          
          # Custom build phase to build both part1 and part2
          buildPhase = ''
            runHook preBuild
            mkdir -p $out/bin
            go build -o $out/bin/part1 part1.go common.go
            go build -o $out/bin/part2 part2.go common.go
            runHook postBuild
          '';
          
          # Skip install phase since we handle it in buildPhase
          installPhase = ''
            runHook preInstall
            # Binaries already in $out/bin from buildPhase
            runHook postInstall
          '';
          
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
            program = "${aoc-solution}/bin/part1";
          };
          part2 = {
            type = "app";
            program = "${aoc-solution}/bin/part2";
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
