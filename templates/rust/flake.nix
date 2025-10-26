{
  description = "Advent of Code solution in Rust";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Build the Rust package using buildRustPackage
        aoc-solution = pkgs.rustPlatform.buildRustPackage {
          pname = "aoc-solution";
          version = "0.1.0";
          
          src = ./.;
          
          cargoLock = {
            lockFile = ./Cargo.lock;
          };
          
          meta = with pkgs.lib; {
            description = "Advent of Code solution in Rust";
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
            rustc
            cargo
            rustfmt
            rust-analyzer
            clippy
          ];

          shellHook = ''
            echo "Rust environment ready"
          '';
        };
      }
    );
}
