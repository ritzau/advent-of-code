{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    nim
    nimlsp
  ];

  shellHook = ''
    echo "Nim environment ready"
  '';
}
