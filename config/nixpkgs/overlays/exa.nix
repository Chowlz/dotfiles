self: super: {
  exa = super.exa.overrideAttrs (old: rec {
    src = super.fetchFromGitHub {
      owner = "ogham";
      repo = "exa";
      rev = "a7aca35d977c52557fe4c8d5dbeba68990e402ac";
      sha256 = "1cjqmf03pwmbclzwcnk0ykj1cmai0qd73rnvh5y8q1kq7qxbs2y7";
    };

    postPatch = super.lib.optionalString (! super.stdenv.isDarwin) ''
      substituteInPlace src/output/table.rs \
      --replace "/usr/share/zoneinfo" "/etc/zoneinfo"
    '';

    postInstall = ''
      pandoc --standalone -f markdown -t man man/exa.1.md > man/exa.1
      pandoc --standalone -f markdown -t man man/exa_colors.5.md > man/exa_colors.5
      installManPage man/exa.1 man/exa_colors.5
      installShellCompletion \
        --bash --name exa completions/bash/exa \
        --fish --name exa.fish completions/fish/exa.fish \
        --zsh --name _exa completions/zsh/_exa
    '';


    patches = [ ];

    cargoDeps = old.cargoDeps.overrideAttrs (super.lib.const {
      inherit src patches;
      outputHash = "0y1czqsr5wfbpg671970lx4dmxk158h27hrynyai7y9vss77i82f";
    });
  });
}
