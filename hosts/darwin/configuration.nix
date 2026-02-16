{ config, pkgs, lib, ... }:

let
  packages = import ../../common/packages.nix pkgs;
in {
  # Packages
  environment.systemPackages =
    packages.base.common ++
    packages.base.gnu-utils ++
    packages.infra.homelab ++
    packages.infra.kubernetes ++
    packages.languages.clojure ++
    packages.languages.go ++
    packages.languages.nodejs;
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
  # Fonts
  fonts.packages = with pkgs; [
    nerd-fonts.sauce-code-pro
  ];
  # Shells
  environment.shells = [ pkgs.bash pkgs.zsh pkgs.fish ];
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish = {
    enable = true;
    useBabelfish = true;
    babelfishPackage = pkgs.babelfish;
    # Fix PATH issues caused by macOS's /usr/libexec/path_helper
    # shellInit = "set -gx PATH ${concatStringsSep " " (splitString ":" config.environment.systemPath)}";
  };
}