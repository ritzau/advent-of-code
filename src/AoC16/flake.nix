{
  description = "Advent of Code 2016 - All days and languages";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Day 1 implementations in different languages
    s16e01-go = {
      url = "path:./s16e01-go";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-kotlin = {
      url = "path:./s16e01-kotlin";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-nim = {
      url = "path:./s16e01-nim";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-python = {
      url = "path:./s16e01-python";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-rust = {
      url = "path:./s16e01-rust";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-typescript = {
      url = "path:./s16e01-typescript";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-zig = {
      url = "path:./s16e01-zig";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        # Aggregate checks from all sub-flakes
        checks =
          inputs.s16e01-go.checks.${system} //
          inputs.s16e01-kotlin.checks.${system} //
          inputs.s16e01-nim.checks.${system} //
          inputs.s16e01-python.checks.${system} //
          inputs.s16e01-rust.checks.${system} //
          inputs.s16e01-typescript.checks.${system} //
          inputs.s16e01-zig.checks.${system};

        # Aggregate packages from all sub-flakes
        packages = {
          s16e01-go = inputs.s16e01-go.packages.${system}.default;
          s16e01-kotlin = inputs.s16e01-kotlin.packages.${system}.default;
          s16e01-nim = inputs.s16e01-nim.packages.${system}.default;
          s16e01-python = inputs.s16e01-python.packages.${system}.default;
          s16e01-rust = inputs.s16e01-rust.packages.${system}.default;
          s16e01-typescript = inputs.s16e01-typescript.packages.${system}.default;
          s16e01-zig = inputs.s16e01-zig.packages.${system}.default;
        };

        # Development shell for the whole year
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Add any year-wide tools here
          ];

          shellHook = ''
            echo "🎄 Advent of Code 2016 - All Solutions"
            echo ""
            echo "Available implementations:"
            echo "  Day 1: Go, Kotlin, Nim, Python, Rust, TypeScript, Zig"
            echo ""
            echo "Commands:"
            echo "  nix flake check              - Run all checks for all days/languages"
            echo "  nix build .#s16e01-rust      - Build specific day/language"
            echo "  nix run .#s16e01-rust        - Run specific day/language"
            echo ""
            echo "Enter specific day directory to work on that solution"
          '';
        };
      }
    );
}
