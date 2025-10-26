{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    go
    gopls
  ];

  shellHook = ''
    echo "Go environment ready"
  '';
}
