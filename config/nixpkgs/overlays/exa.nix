self: super: {
  exa = super.exa.overrideAttrs (old: rec {
    src = super.fetchFromGitHub {
      owner = "ogham";
      repo = "exa";
      rev = "95682f567461beca22198b29a7abf9411f41189f";
      sha256 = "1abbk56xvaif5vsl6z4arr9jj8cxnfklql43215hckl0b7v2hhj5";
    };

    postPatch = super.lib.optionalString (! super.stdenv.isDarwin) ''
      substituteInPlace src/output/table.rs \
      --replace "/usr/share/zoneinfo" "/etc/zoneinfo"
    '';

    patches = [ ];

    cargoDeps = old.cargoDeps.overrideAttrs (super.lib.const {
      inherit src patches;
      outputHash = "154vb2l6778iriq6s4yix8sy2sl21jk87rmkyfrmjaa4cbqprj90";
    });
  });
}
