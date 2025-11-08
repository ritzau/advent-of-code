{ lib, stdenv, jdk, gradle_8, ktlint, callPackage, makeWrapper }:
let
  buildMavenRepo = callPackage ./maven-repo.nix { };

  mavenRepo = buildMavenRepo {
    name = "nix-maven-repo";
    repos = [
      "https://repo1.maven.org/maven2"
      "https://plugins.gradle.org/m2"
      "https://maven.pkg.jetbrains.space/kotlin/p/kotlin/dev"
    ];
    deps = builtins.fromJSON (builtins.readFile ./deps.json);
  };
in stdenv.mkDerivation {
  pname = "s16e01-kotlin";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ gradle_8 ktlint makeWrapper ];

  JDK_HOME = "${jdk.home}";

  buildPhase = ''
    runHook preBuild
    export GRADLE_USER_HOME=$TMP/gradle-home
    export NIX_MAVEN_REPO=${mavenRepo}
    unset GRADLE_OPTS
    gradle distTar -x test \
      --offline --no-daemon \
      --warning-mode=all --parallel \
      -PnixMavenRepo=${mavenRepo}
    runHook postBuild
  '';

  # Only run checks during 'nix flake check', not during regular builds
  doCheck = false;

  passthru.tests = {
    check = stdenv.mkDerivation {
      pname = "s16e01-kotlin-check";
      version = "0.1.0";
      src = ./.;

      nativeBuildInputs = [ gradle_8 ktlint ];

      JDK_HOME = "${jdk.home}";

      buildPhase = ''
        ktlint src/**/*.kt
        export GRADLE_USER_HOME=$TMP/gradle-home
        export NIX_MAVEN_REPO=${mavenRepo}
        unset GRADLE_OPTS
        gradle check \
          --offline --no-daemon \
          --warning-mode=all --parallel \
          -PnixMavenRepo=${mavenRepo}

        # Create a marker file to indicate success
        touch $out
      '';

      installPhase = "true";
    };
  };

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin

    # Extract the tar distribution created by distTar
    tar -xf build/distributions/*.tar -C $out --strip-components=1

    # Remove Windows batch files
    rm -f $out/bin/*.bat

    # Create part1 and part2 runner scripts
    cat > $out/bin/s16e01-kotlin-part1 << 'EOF'
#!/bin/sh
exec ${jdk}/bin/java -cp $out/lib/'*' Part1Kt "$@"
EOF
    chmod +x $out/bin/s16e01-kotlin-part1

    cat > $out/bin/s16e01-kotlin-part2 << 'EOF'
#!/bin/sh
exec ${jdk}/bin/java -cp $out/lib/'*' Part2Kt "$@"
EOF
    chmod +x $out/bin/s16e01-kotlin-part2

    # Wrap all scripts to use the correct JDK
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
