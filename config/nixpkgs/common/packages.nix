{ pkgs, ... }:

{
  aws = with pkgs; [
    awscli2
    nodePackages.aws-cdk
    python311Packages.cfn-lint
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
    rcm
    ripgrep
    shellcheck
    tmux
    unixtools.watch
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