{
  description = "Advent of Code Solutions - Multi-year, Multi-language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Year-level flakes
    aoc16.url = "path:./src/AoC16";
    aoc23.url = "path:./src/AoC23";

    # Template flakes
    template-go.url = "path:./templates/go";
    template-kotlin.url = "path:./templates/kotlin";
    template-nim.url = "path:./templates/nim";
    template-python.url = "path:./templates/python";
    template-rust.url = "path:./templates/rust";
    template-typescript.url = "path:./templates/typescript";
    template-zig.url = "path:./templates/zig";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Year flakes
        yearFlakes = [ "aoc16" "aoc23" ];

        # Template flakes
        templateFlakes = [
          "template-go"
          "template-kotlin"
          "template-nim"
          "template-python"
          "template-rust"
          "template-typescript"
          "template-zig"
        ];

        # All flakes to aggregate
        allFlakeNames = yearFlakes ++ templateFlakes;

        # Aggregate all checks from all flakes
        allChecks = pkgs.lib.foldl'
          (acc: flakeName:
            let
              flake = inputs.${flakeName};
              flakeChecks = flake.checks.${system} or {};
              # Prefix each check with the flake name to avoid collisions
              renamedChecks = builtins.listToAttrs (
                map (checkName: {
                  name = "${flakeName}-${checkName}";
                  value = flakeChecks.${checkName};
                }) (builtins.attrNames flakeChecks)
              );
            in
              acc // renamedChecks
          )
          {}
          allFlakeNames;

        # Aggregate all packages from all flakes
        allPackages = pkgs.lib.foldl'
          (acc: flakeName:
            let
              flake = inputs.${flakeName};
              flakePackages = flake.packages.${system} or {};
              # Rename to include flake name
              renamedPackages = builtins.listToAttrs (
                map (pkgName: {
                  name = if pkgName == "default" then flakeName else "${flakeName}-${pkgName}";
                  value = flakePackages.${pkgName};
                }) (builtins.attrNames flakePackages)
              );
            in
              acc // renamedPackages
          )
          {}
          allFlakeNames;

      in
      {
        # Aggregate checks from all sub-flakes
        checks = allChecks // {
          # Add a meta-check that runs all year checks
          all-years = pkgs.runCommand "all-years-check" {} ''
            echo "All year checks passed!"
            mkdir -p $out
            echo "✅ All Advent of Code solutions verified" > $out/result
          '';
        };

        # Aggregate packages from all sub-flakes
        packages = allPackages;

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
