{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    argocd
    awscli2
    babashka
    bat
    bat-extras.batdiff
    bat-extras.batgrep
    bat-extras.batman
    bat-extras.batwatch
    clj-kondo
    clojure
    coreutils
    diceware
    diffutils
    eza
    findutils
    gawk
    git
    git-lfs
    gnugrep
    gnused
    gnutar
    gzip
    inetutils
    ispell
    jdk
    jq
    keychain
    kind
    kubectl
    kubernetes-helm
    neofetch
    neovim
    nixpkgs-fmt
    nodejs
    nodePackages.aws-cdk
    nodePackages.pnpm
    openssh
    pastel
    python311Packages.cfn-lint
    rcm
    ripgrep
    rlwrap
    shellcheck
    tmux
    unixtools.watch
    wget
    which
    xz
    yq-go
  ];
}