{ config, pkgs, lib, ... }:

let
  unstable = import <nixpkgs-unstable> {};
in {
  home.username = "ccruz";
  home.homeDirectory = "/home/ccruz";
  home.stateVersion = "21.03";
  home.packages = [
    unstable.awscli
    unstable.bat
    unstable.bat-extras.batman
    unstable.bat-extras.batdiff
    unstable.bat-extras.batgrep
    unstable.bat-extras.batwatch
    unstable.clj-kondo
    unstable.clojure
    unstable.exa
    unstable.bpytop
    unstable.emacs
    unstable.neofetch
    unstable.neovim
    unstable.nodejs
    unstable.rcm
    unstable.rlwrap
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
