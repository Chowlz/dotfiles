self: super: {
  exa = super.exa.overrideAttrs (old: rec {
    src = super.fetchFromGitHub {
      owner = "ogham";
      repo = "exa";
      rev = "13b91cced4cab012413b25c9d3e30c63548639d0";
      sha256 = "18y4v1s102lh3gvgjwdd66qlsr75wpwpcj8zsk5y5r95a405dkfm";
    };

    patches = [ ];
    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ self.just self.pandoc ];

    postPatch = super.lib.optionalString (! super.stdenv.isDarwin) ''
      substituteInPlace src/output/table.rs \
      --replace "/usr/share/zoneinfo" "/etc/zoneinfo"
    '';

    postInstall = ''
      just man
      installManPage target/man/exa.1
      installShellCompletion \
        --name exa completions/completions.bash \
        --name exa.fish completions/completions.fish \
        --name _exa completions/completions.zsh
    '';

    cargoDeps = old.cargoDeps.overrideAttrs (super.lib.const {
      inherit src;
      outputHash = "16hivdxphyddrh6j3hs9nvf91dhrjwlr68xs9z36n3b88wadxjcx";
    });
  });
}
