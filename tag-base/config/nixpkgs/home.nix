{ config, pkgs, lib, ... }:

let
  unstable = import <nixpkgs-unstable> {};
  stdenv = pkgs.stdenv;
in {
  home.username = "ccruz";
  home.homeDirectory = "/home/ccruz";
  home.stateVersion = "21.03";
  home.packages = [
    unstable.awscli
    unstable.bat
    unstable.bat-extras.batdiff
    unstable.bat-extras.batgrep
    unstable.bat-extras.batman
    unstable.bat-extras.batwatch
    unstable.clj-kondo
    unstable.clojure
    unstable.diceware
    unstable.exa
    unstable.lastpass-cli
    unstable.neofetch
    unstable.neovim
    unstable.nodejs
    unstable.pastel
    unstable.python39
    unstable.rcm
    unstable.ripgrep
    unstable.rlwrap
  ] ++ lib.optionals stdenv.isDarwin [
    unstable.coreutils
    unstable.curl
    unstable.diffutils
    unstable.findutils
    unstable.gawk
    unstable.gnugrep
    unstable.gnused
    unstable.gnutar
    unstable.gzip
    unstable.keychain
    unstable.lzma
    unstable.mosh
    unstable.tmux
    unstable.wget
    unstable.which
    unstable.zsh
    unstable.zsh-powerlevel10k
  ] ++ lib.optionals stdenv.isLinux [
    unstable.bpytop
    unstable.emacs
    unstable.shellcheck
  ];

  programs.home-manager.enable = true;

  imports = [
    ./programs/fish/fish.nix
    ./programs/nodejs/npm.nix
    ./programs/tmux/tmux.nix
    ./programs/vim/vim.nix
    ./programs/zsh/zsh.nix
  ];
}
