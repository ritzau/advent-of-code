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

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    ktlint src/**/*.kt
    export GRADLE_USER_HOME=$TMP/gradle-home
    export NIX_MAVEN_REPO=${mavenRepo}
    unset GRADLE_OPTS
    gradle check \
      --offline --no-daemon \
      --warning-mode=all --parallel \
      -PnixMavenRepo=${mavenRepo}
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out

    # Extract the tar distribution created by distTar
    tar -xf build/distributions/*.tar -C $out --strip-components=1

    # Wrap the start script to use the correct JDK
    wrapProgram $out/bin/s16e01-kotlin \
      --set JAVA_HOME "${jdk.home}" \
      --prefix PATH : "${jdk}/bin"

    runHook postInstall
  '';

  dontStrip = true;
}
