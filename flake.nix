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
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        # Aggregate checks from all template flakes
        checks =
          (inputs.template-go.checks.${system} or {}) //
          (inputs.template-kotlin.checks.${system} or {}) //
          (inputs.template-nim.checks.${system} or {}) //
          (inputs.template-python.checks.${system} or {}) //
          (inputs.template-rust.checks.${system} or {}) //
          (inputs.template-typescript.checks.${system} or {}) //
          (inputs.template-zig.checks.${system} or {});

        # Aggregate packages from all template flakes
        packages =
          let
            mkPackage = name: inputs.${name}.packages.${system}.default or null;
          in
          pkgs.lib.filterAttrs (_: v: v != null) {
            template-go = mkPackage "template-go";
            template-kotlin = mkPackage "template-kotlin";
            template-nim = mkPackage "template-nim";
            template-python = mkPackage "template-python";
            template-rust = mkPackage "template-rust";
            template-typescript = mkPackage "template-typescript";
            template-zig = mkPackage "template-zig";
          };

        # Development shell for the whole repository
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
            echo "  Go, Kotlin, Nim, Python, Rust, TypeScript, Zig"
            echo ""
            echo "Commands:"
            echo "  nix flake check              - Check all templates"
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
