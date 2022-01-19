{ config, pkgs, lib, ... }:

with lib;
let
  stable-pkgs = import <nixpkgs-stable> {};
  darwin-config = "$HOME/.config/nixpkgs/darwin-configuration.nix";
in {
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin-configuration.nix
  environment.darwinConfig = darwin-config;
  environment.systemPackages = [
    pkgs.argocd
    pkgs.babashka
    pkgs.clojure
    pkgs.coreutils
    pkgs.curl
    pkgs.diffutils
    pkgs.findutils
    pkgs.emacsMacport
    pkgs.gawk
    pkgs.git
    pkgs.gnugrep
    pkgs.gnused
    pkgs.gnutar
    pkgs.gzip
    pkgs.ispell
    stable-pkgs.jdk
    pkgs.jq
    pkgs.keychain
    pkgs.kubectl
    pkgs.kubernetes-helm
    pkgs.lzma
    pkgs.minikube
    pkgs.mosh
    pkgs.openssh
    pkgs.tmux
    pkgs.wget
    pkgs.which
  ];

  nix.nixPath = [
    { darwin-config = darwin-config; }
    "$HOME/.nix-defexpr/channels"
  ];

  # Fonts
  fonts = {
    enableFontDir = true;
    fonts = [ pkgs.nerdfonts ];
  };

  # Shells
  environment.shells = [ pkgs.bash pkgs.zsh pkgs.fish ];
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish = {
    enable = true;
    useBabelfish = true;
    babelfishPackage = pkgs.babelfish;

    # Fix PATH issues caused by macOS's /usr/libexec/path_helper
    shellInit = "set -gx PATH ${concatStringsSep " " (splitString ":" config.environment.systemPath)}";
  };

  # Modules
  imports = [
    ~/.config/nixpkgs/external
  ];
}
