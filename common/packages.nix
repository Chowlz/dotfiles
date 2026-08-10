{ pkgs, ... }:

with pkgs; {
  base = {
    common = [
      bat
      bat-extras.batdiff
      bat-extras.batgrep
      bat-extras.batman
      bat-extras.batwatch
      bats
      btop
      dig
      eza
      gawk
      git
      git-lfs
      gnugrep
      gnused
      gnutar
      gzip
      ispell
      jq
      keychain
      neovim
      nixpkgs-fmt
      nodejs
      pastel
      postgresql
      ripgrep
      shellcheck
      sqlite
      tmux
      unixtools.watch
      unzip
      wget
      which
      xz
      yarn-berry
      yq-go
      zstd
    ];
    gnu-utils = [
      coreutils
      diffutils
      findutils
    ];
    net-utils = [
      inetutils
      openssh
    ];
  };
  infra = {
    ansible = [
      ansible-lint
      check-jsonschema
      jinja2-cli
      (pkgs.python312.withPackages(ps: [
        ps.ansible-core
        ps.django
        ps.jmespath
        ps.pip
      ]))
    ];
    aws = [
      awscli2
      nodePackages.aws-cdk
      python312Packages.cfn-lint
    ];
    homelab = [
      exiftool
      immich-go
      rclone
    ];
    kubernetes = [
      kubectl
      kubernetes-helm
    ];
  };
  languages = {
    clojure = [
      babashka
      clj-kondo
      clojure
      jdk
      rlwrap
    ];
    go = [
      go
    ];
    nodejs = [
      nodejs
    ];
  };
}