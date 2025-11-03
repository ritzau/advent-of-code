{
  description = "Advent of Code test runner";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build the Go package
        package = pkgs.buildGoModule {
          pname = "aoc";
          version = "0.1.0";
          src = ./.;

          vendorHash = "sha256-0EBm5Mkyp0MUHoDxPhCufbF7t0cipirRUbxPTAcLQN4=";

          ldflags = [ "-s" "-w" ];
        };
      in
      {
        packages = {
          default = package;
        };

        apps = {
          default = {
            type = "app";
            program = "${package}/bin/aoc";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            go
            gopls
            nix
          ];

          shellHook = ''
            echo "🎄 AoC Runner development environment"
            echo ""
            echo "Available commands:"
            echo "  go build       - Build locally"
            echo "  go test ./...  - Run tests"
            echo "  nix build      - Build with Nix"
            echo "  nix run        - Run the runner"
          '';
        };
      }
    );
}
