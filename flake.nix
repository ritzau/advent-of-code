{
  description = "Advent of Code Templates";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Template flakes
    template-go = {
      url = "path:./templates/go";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    template-haskell = {
      url = "path:./templates/haskell";
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
      # Don't follow nixpkgs - Zig template uses its own pinned version (24.05) for zig_0_12
      inputs.flake-utils.follows = "flake-utils";
    };

    # Legacy Haskell solutions
    aoc21 = {
      url = "path:./src/AoC21";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    aoc22 = {
      url = "path:./src/AoC22";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    aoc23 = {
      url = "path:./src/AoC23";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    # 2016 Day 1 Solutions (multi-language implementations)
    s16e01-go = {
      url = "path:./src/AoC16/s16e01-go";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-haskell = {
      url = "path:./src/AoC16/s16e01-haskell";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-kotlin = {
      url = "path:./src/AoC16/s16e01-kotlin";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-nim = {
      url = "path:./src/AoC16/s16e01-nim";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-python = {
      url = "path:./src/AoC16/s16e01-python";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-rust = {
      url = "path:./src/AoC16/s16e01-rust";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-typescript = {
      url = "path:./src/AoC16/s16e01-typescript";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    s16e01-zig = {
      url = "path:./src/AoC16/s16e01-zig";
      # Don't follow nixpkgs - Zig uses its own pinned version for zig_0_12
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    let
      # All project flakes - everything except self and system inputs
      projectFlakes = builtins.filter
        (name: name != "self" && name != "nixpkgs" && name != "flake-utils")
        (builtins.attrNames inputs);
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Aggregate checks from all project flakes
        allChecks = pkgs.lib.foldl'
          (acc: name: acc // (inputs.${name}.checks.${system} or {}))
          {}
          projectFlakes;

        # Aggregate packages from all project flakes
        allPackages = pkgs.lib.foldl'
          (acc: name:
            let pkg = inputs.${name}.packages.${system}.default or null;
            in if pkg != null then acc // { ${name} = pkg; } else acc
          )
          {}
          projectFlakes;
      in
      {
        checks = allChecks;
        packages = allPackages;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            git
            just
            nixpkgs-fmt
          ];

          shellHook = ''
            echo "🎄 Advent of Code Templates"
            echo ""
            echo "Available templates:"
            echo "  Go, Haskell, Kotlin, Nim, Python, Rust, TypeScript, Zig"
            echo ""
            echo "Solutions:"
            echo "  src/AoC16/s16e01-*         - 2016 Day 1 (all 8 languages)"
            echo "  src/AoC21                 - 2021 solutions (Haskell)"
            echo "  src/AoC22                 - 2022 solutions (Haskell)"
            echo "  src/AoC23                 - 2023 solutions (TypeScript)"
            echo ""
            echo "Commands:"
            echo "  nix flake check              - Check all templates and solutions"
            echo "  nix build                    - Build all solution packages"
            echo "  cd templates/<lang>          - Work on specific template"
            echo ""
            echo "Just commands:"
            echo "  just new <year> <day> <lang> - Create new solution from template"
            echo ""
          '';
        };
      }
    );
}
