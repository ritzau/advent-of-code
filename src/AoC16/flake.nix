{
  description = "Advent of Code 2016 - All days and languages";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Day 1 implementations in different languages
    s16e01-go.url = "path:./s16e01-go";
    s16e01-kotlin.url = "path:./s16e01-kotlin";
    s16e01-nim.url = "path:./s16e01-nim";
    s16e01-python.url = "path:./s16e01-python";
    s16e01-rust.url = "path:./s16e01-rust";
    s16e01-typescript.url = "path:./s16e01-typescript";
    s16e01-zig.url = "path:./s16e01-zig";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # List of all day flakes (excluding nixpkgs and flake-utils)
        dayFlakes = builtins.removeAttrs inputs [ "self" "nixpkgs" "flake-utils" ];

        # Aggregate all checks from all day flakes
        allChecks = pkgs.lib.foldl'
          (acc: flakeName:
            let
              flake = inputs.${flakeName};
              flakeChecks = flake.checks.${system} or {};
              # Prefix each check with the flake name to avoid collisions
              prefixedChecks = builtins.mapAttrs
                (checkName: checkValue: checkValue)
                flakeChecks;
              # Rename to include flake name
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
          (builtins.attrNames dayFlakes);

        # Aggregate all packages from all day flakes
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
          (builtins.attrNames dayFlakes);

      in
      {
        # Aggregate checks from all sub-flakes
        checks = allChecks;

        # Aggregate packages from all sub-flakes
        packages = allPackages;

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
