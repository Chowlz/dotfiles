{ pkgs, ... }:

{
  ansible = with pkgs; [
    ansible-lint
    (pkgs.python312.withPackages(ps: [
      ps.ansible-core
    ]))
  ];
  aws = with pkgs; [
    awscli2
    nodePackages.aws-cdk
    python312Packages.cfn-lint
  ];
  clojure = with pkgs; [
    babashka
    clj-kondo
    clojure
    jdk
    rlwrap
  ];
  common = with pkgs; [
    bat
    bat-extras.batdiff
    bat-extras.batgrep
    bat-extras.batman
    bat-extras.batwatch
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
    process-compose
    rcm
    ripgrep
    shellcheck
    tmux
    unixtools.watch
    unzip
    wget
    which
    xz
    yq-go
  ];
  kubernetes = with pkgs; [
    kind
    kubectl
    kubernetes-helm
  ];
  os = with pkgs; [
    coreutils
    diffutils
    findutils
    inetutils
  ];
  ssh = with pkgs; [
    openssh
  ];
}