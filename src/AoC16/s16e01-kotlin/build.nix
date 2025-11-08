{ lib, stdenv, jdk, kotlin, ktlint, makeWrapper }:

stdenv.mkDerivation {
  pname = "s16e01-kotlin";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ kotlin jdk makeWrapper ];

  buildPhase = ''
    runHook preBuild

    # Compile Kotlin sources
    mkdir -p classes
    kotlinc -d classes \
      src/main/kotlin/Common.kt \
      src/main/kotlin/Main.kt \
      src/main/kotlin/Part1.kt \
      src/main/kotlin/Part2.kt

    runHook postBuild
  '';

  # Only run checks during 'nix flake check', not during regular builds
  doCheck = false;

  passthru.tests = {
    check = stdenv.mkDerivation {
      pname = "s16e01-kotlin-check";
      version = "0.1.0";
      src = ./.;

      nativeBuildInputs = [ kotlin jdk ktlint ];

      buildPhase = ''
        # Run ktlint
        ktlint src/**/*.kt

        # Compile and run tests
        mkdir -p classes
        kotlinc -d classes \
          src/main/kotlin/Common.kt \
          src/test/kotlin/CommonTest.kt

        # Run tests
        java -cp classes:${kotlin}/lib/kotlin-stdlib.jar CommonTestKt

        # Create a marker file to indicate success
        touch $out
      '';

      installPhase = "true";
    };
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/lib

    # Copy compiled classes
    cp -r classes/* $out/lib/

    # Create main runner script
    cat > $out/bin/s16e01-kotlin << 'EOF'
#!/bin/sh
exec ${jdk}/bin/java -cp $out/lib:${kotlin}/lib/kotlin-stdlib.jar MainKt "$@"
EOF
    chmod +x $out/bin/s16e01-kotlin

    # Create part1 runner script
    cat > $out/bin/s16e01-kotlin-part1 << 'EOF'
#!/bin/sh
exec ${jdk}/bin/java -cp $out/lib:${kotlin}/lib/kotlin-stdlib.jar Part1Kt "$@"
EOF
    chmod +x $out/bin/s16e01-kotlin-part1

    # Create part2 runner script
    cat > $out/bin/s16e01-kotlin-part2 << 'EOF'
#!/bin/sh
exec ${jdk}/bin/java -cp $out/lib:${kotlin}/lib/kotlin-stdlib.jar Part2Kt "$@"
EOF
    chmod +x $out/bin/s16e01-kotlin-part2

    # Wrap all scripts to ensure proper environment
    wrapProgram $out/bin/s16e01-kotlin \
      --set JAVA_HOME "${jdk.home}" \
      --prefix PATH : "${jdk}/bin"

    wrapProgram $out/bin/s16e01-kotlin-part1 \
      --set JAVA_HOME "${jdk.home}" \
      --prefix PATH : "${jdk}/bin"

    wrapProgram $out/bin/s16e01-kotlin-part2 \
      --set JAVA_HOME "${jdk.home}" \
      --prefix PATH : "${jdk}/bin"

    runHook postInstall
  '';

  dontStrip = true;
}
