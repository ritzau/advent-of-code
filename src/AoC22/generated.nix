{ mkDerivation, base, lib }:
mkDerivation {
  pname = "aoc22";
  version = "22.15.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [ base ];
  description = "Advent of Code 2022 Solutions";
  license = "unknown";
  mainProgram = "aoc22";
}
