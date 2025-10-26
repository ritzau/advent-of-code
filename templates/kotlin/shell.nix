{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    kotlin
    kotlin-language-server
  ];

  shellHook = ''
    echo "Kotlin environment ready"
  '';
}
