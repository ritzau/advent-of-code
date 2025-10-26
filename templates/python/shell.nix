{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    python3
    python3Packages.pytest
  ];

  shellHook = ''
    echo "Python environment ready"
  '';
}
