{ mkDerivation, base, containers, HUnit, lib }:
mkDerivation {
  pname = "aoc-solution";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers ];
  executableHaskellDepends = [ base containers ];
  testHaskellDepends = [ base containers HUnit ];
  description = "Advent of Code solution";
  license = "unknown";
}
