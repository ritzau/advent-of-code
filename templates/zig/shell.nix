{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    zig
    zls
  ];

  shellHook = ''
    echo "Zig environment ready"
  '';
}
