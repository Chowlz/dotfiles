{ config, pkgs, lib, ... }:

with lib;
let
  darwin-config = "$HOME/.config/nixpkgs/darwin-configuration.nix";
  clojure = pkgs.clojure.override {
    # Set to open jdk 11
    jdk = pkgs.openjdk11;
  };
  minikube = let
    version = "1.29.0";
    src = pkgs.fetchFromGitHub {
      owner = "kubernetes";
      repo = "minikube";
      rev = "v${version}";
      sha256 = "sha256-rdcMgL7bzdlxrelui+V1APJik0v/4YyUqj9QlMRq1nI=";
    };
    in (pkgs.minikube.override rec {
      buildGoModule = args: pkgs.buildGoModule.override {} (args // {
        inherit src version;
        buildPhase = ''
          make COMMIT=${src.rev}
        '';
        vendorSha256 = "sha256-wRCSUDzz+1e4/ijwAnIM8a/AlnNNdVkiz3WO4Nhuy+M=";
      });
    });
in {
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin-configuration.nix
  environment.darwinConfig = darwin-config;
  environment.systemPackages = [
    clojure
    minikube
    pkgs.argocd
    pkgs.babashka
    pkgs.coreutils
    pkgs.clj-kondo
    pkgs.diffutils
    pkgs.findutils
  #  pkgs.emacsMacport
    pkgs.gawk
    pkgs.git
    pkgs.gnugrep
    pkgs.gnused
    pkgs.gnutar
    pkgs.gzip
    pkgs.ispell
    pkgs.iterm2
    pkgs.jdk11
    pkgs.jq
    pkgs.keychain
    pkgs.kubectl
    pkgs.kubernetes-helm
    pkgs.xz
    pkgs.mosh
    pkgs.nodejs
    pkgs.openssh
    pkgs.tmux
    pkgs.unixtools.watch
    pkgs.wget
    pkgs.which
  ];

  nix.nixPath = [
    { darwin-config = darwin-config; }
    "$HOME/.nix-defexpr/channels"
  ];

  # Fonts
  fonts = {
    fontDir.enable = true;
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
