{ mkDerivation, base, lib }:
mkDerivation {
  pname = "aoc21";
  version = "21.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [ base ];
  description = "Advent of Code 2021 Solutions";
  license = "unknown";
  mainProgram = "aoc21";
}
