self: super: {
  mosh = super.mosh.overrideAttrs (old: rec {
    src = super.fetchFromGitHub {
      owner = "mobile-shell";
      repo = "mosh";
      rev = "03087e7a761df300c2d8cd6e072890f8e1059dfa";
      sha256 = "170m3q9sxw6nh8fvrf1l0hbx0rjjz5f5lzhd41143kd1rps3liw8";
    };

    patches = [
      ./mosh/configure.ac.patch
      ./mosh/scripts/mosh.pl.patch
      ./mosh/src/frontend/mosh-server.cc.patch
    ];

    postPatch = ''
      substituteInPlace scripts/mosh.pl \
          --subst-var-by ssh "${super.openssh}/bin/ssh"
      substituteInPlace scripts/mosh.pl \
          --subst-var-by mosh-client "$out/bin/mosh-client"
    '';
  });
}
