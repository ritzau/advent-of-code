{
  description = "Advent of Code Solutions - Multi-year, Multi-language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Year-level flakes
    aoc16 = {
      url = "path:./src/AoC16";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    aoc23 = {
      url = "path:./src/AoC23";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    # Template flakes
    template-go = {
      url = "path:./templates/go";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-kotlin = {
      url = "path:./templates/kotlin";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-nim = {
      url = "path:./templates/nim";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-python = {
      url = "path:./templates/python";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-rust = {
      url = "path:./templates/rust";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-typescript = {
      url = "path:./templates/typescript";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-zig = {
      url = "path:./templates/zig";
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
        # Aggregate checks from all sub-flakes - just merge them all
        checks =
          inputs.aoc16.checks.${system} //
          inputs.aoc23.checks.${system} //
          inputs.template-go.checks.${system} //
          inputs.template-kotlin.checks.${system} //
          inputs.template-nim.checks.${system} //
          inputs.template-python.checks.${system} //
          inputs.template-rust.checks.${system} //
          inputs.template-typescript.checks.${system} //
          inputs.template-zig.checks.${system};

        # Aggregate packages from all sub-flakes
        packages = {
          # Year packages
          aoc16 = inputs.aoc16.packages.${system};
          aoc23 = inputs.aoc23.packages.${system}.default;

          # Template packages
          template-go = inputs.template-go.packages.${system}.default;
          template-kotlin = inputs.template-kotlin.packages.${system}.default;
          template-nim = inputs.template-nim.packages.${system}.default;
          template-python = inputs.template-python.packages.${system}.default;
          template-rust = inputs.template-rust.packages.${system}.default;
          template-typescript = inputs.template-typescript.packages.${system}.default;
          template-zig = inputs.template-zig.packages.${system}.default;
        };

        # Development shell for the whole repository
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            git
            just
            nixpkgs-fmt
          ];

          shellHook = ''
            echo "🎄 Advent of Code Solutions - Repository Root"
            echo ""
            echo "Available years:"
            echo "  2016 (AoC16): Multiple languages - Day 1"
            echo "  2023 (AoC23): TypeScript - Multiple days"
            echo "  2021 (AoC21): Haskell - Day 1 (legacy, no flake)"
            echo "  2022 (AoC22): Haskell - Multiple days (legacy, no flake)"
            echo ""
            echo "Available templates:"
            echo "  Go, Kotlin, Nim, Python, Rust, TypeScript, Zig"
            echo ""
            echo "Commands:"
            echo "  nix flake check              - Run ALL checks (all years, all days, all languages)"
            echo "  nix flake check .#aoc16-*    - Check specific year"
            echo "  nix flake check .#template-* - Check specific template"
            echo ""
            echo "  cd src/AoC16 && nix flake check  - Check all 2016 solutions"
            echo "  cd src/AoC23 && nix flake check  - Check all 2023 solutions"
            echo ""
            echo "Just commands:"
            echo "  just test      - Run tests for current directory"
            echo "  just run       - Run solution for current directory"
            echo ""
          '';
        };
      }
    );
}
