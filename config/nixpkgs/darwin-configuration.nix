{ config, pkgs, ... }:

let
  darwin-config = "$HOME/.config/nixpkgs/darwin-configuration.nix";
in {
  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin-configuration.nix
  environment.darwinConfig = darwin-config;
  environment.systemPackages = [
    pkgs.coreutils
    pkgs.curl
    pkgs.diffutils
    pkgs.findutils
    pkgs.gawk
    pkgs.git
    pkgs.gnugrep
    pkgs.gnused
    pkgs.gnutar
    pkgs.gzip
    pkgs.ispell
    pkgs.keychain
    pkgs.lzma
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

  # Shells
  environment.shells = [ pkgs.bash pkgs.zsh pkgs.fish ];
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish.enable = true;
  programs.fish.useBabelfish = true;
  programs.fish.babelfishPackage = pkgs.babelfish;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
