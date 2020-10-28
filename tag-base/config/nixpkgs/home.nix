{ config, pkgs, lib, ... }:

let
  unstable = import <nixpkgs-unstable> {};
  exa = pkgs.callPackage ./pkgs/exa/exa.nix {};
in
{
  home.username = "ccruz";
  home.homeDirectory = "/home/ccruz";
  home.stateVersion = "20.09";
  home.packages =
  [
    exa
    unstable.awscli
    unstable.bat
    unstable.bat-extras.batman
    unstable.bat-extras.batdiff
    unstable.bat-extras.batgrep
    unstable.bat-extras.batwatch
    unstable.clojure
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
    ./programs/vim/vim.nix
  ];
}
