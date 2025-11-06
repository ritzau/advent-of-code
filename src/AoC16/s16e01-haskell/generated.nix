{ mkDerivation, base, containers, HUnit, lib, split }:
mkDerivation {
  pname = "aoc-solution";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers split ];
  executableHaskellDepends = [ base containers split ];
  testHaskellDepends = [ base containers HUnit split ];
  description = "Advent of Code solution";
  license = "unknown";
}
