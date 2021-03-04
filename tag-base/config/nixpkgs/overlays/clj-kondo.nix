self: super:

{
  clj-kondo = super.clj-kondo.overrideAttrs (old: rec {
    pname = "clj-kondo";
    version = "2021.03.03";

    reflectionJson = super.fetchurl {
      url = "https://raw.githubusercontent.com/borkdude/${pname}/v${version}/reflection.json";
      sha256 = "0412yabsvhjb78bqj81d6c4srabh9xv5vmm93k1fk2abk69ii10b";
    };

    src = super.fetchurl {
      url = "https://github.com/borkdude/${pname}/releases/download/v${version}/${pname}-${version}-standalone.jar";
      sha256 = "1bpb9x691zfzrjpwlh5jgsfyj4sk5192bj3576k64a1cak2jsir8";
    };

    buildInputs = [ super.graalvm11-ce ];

    buildPhase = ''
      native-image  \
        -jar ${src} \
        -H:Name=${pname} \
        ${super.lib.optionalString super.stdenv.isDarwin ''-H:-CheckToolchain''} \
        -H:+ReportExceptionStackTraces \
        -J-Dclojure.spec.skip-macros=true \
        -J-Dclojure.compiler.direct-linking=true \
        "-H:IncludeResources=clj_kondo/impl/cache/built_in/.*" \
        -H:ReflectionConfigurationFiles=${reflectionJson} \
        --initialize-at-build-time  \
        -H:Log=registerResource: \
        --verbose \
        --no-fallback \
        --no-server \
        "-J-Xmx3g"
    '';
  });
}
