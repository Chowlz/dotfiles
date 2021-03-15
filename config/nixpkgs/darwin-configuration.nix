{ config, pkgs, lib, ... }:

with lib;
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
    pkgs.emacsMacport
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
    ./modules/darwin/yabai.nix
  ];
  modules.yabai = {
    enable = true;
    config = {
      windowPlacement = "second_child";
      windowTopmost = "off";
      windowOpacity = "off";
      windowOpacityDuration = 0.0;
      activeWindowOpacity = 1.0;
      windowShadow = "on";
      autoBalance = "off";
      splitRatio = 0.50;
      mouseModifier = "ctrl";
      mouseAction1 = "move";
      mouseAction2 = "resize";
      layout = "bsp";
      topPadding = 2;
      bottomPadding = 2;
      leftPadding = 2;
      rightPadding = 2;
    };
    rules = [
      {
        app = { regex = "^Cisco .*$"; };
        sticky = "on";
        layer = "above";
        manage = "off";
      }
      {
        app = { regex = "^Disk Utility$"; };
        sticky = "on";
        layer = "above";
        manage = "off";
      }
      {
        app = { regex = "^Finder$"; };
        sticky = "on";
        layer = "above";
        manage = "off";
      }
      {
        app = { regex = "^iTerm2$"; };
        manage = "off";
      }
      {
        app = { regex = "^Spotify$"; };
        manage = "off";
      }
      {
        app = { regex = "^System Preferences$"; };
        sticky = "on";
        layer = "above";
        manage = "off";
      }
      {
        app = { regex = "^Terminal$"; };
        manage = "off";
      }
    ];
  };
}
