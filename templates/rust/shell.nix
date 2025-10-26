{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    rustc
    cargo
    rust-analyzer
  ];

  shellHook = ''
    echo "Rust environment ready"
  '';
}
