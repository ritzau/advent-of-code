{ mkDerivation, base, HUnit, lib }:
mkDerivation {
  pname = "s21e01-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base HUnit ];
  description = "Advent of Code solution";
  license = "unknown";
}
