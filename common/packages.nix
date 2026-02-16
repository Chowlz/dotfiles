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
      openssh
      pastel
      postgresql
      ripgrep
      shellcheck
      tmux
      unixtools.watch
      unzip
      wget
      which
      xz
      yq-go
      zstd
    ];
    gnu-utils = [
      coreutils
      diffutils
      findutils
      inetutils
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