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
        # Aggregate checks from all template flakes
        checks =
          inputs.template-go.checks.${system} //
          inputs.template-kotlin.checks.${system} //
          inputs.template-nim.checks.${system} //
          inputs.template-python.checks.${system} //
          inputs.template-rust.checks.${system} //
          inputs.template-typescript.checks.${system} //
          inputs.template-zig.checks.${system};

        # Aggregate packages from all template flakes
        packages = {
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
