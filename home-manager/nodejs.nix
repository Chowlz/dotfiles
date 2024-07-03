{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.modules.nodejs;
in {
  options.modules.nodejs = {
    enable = mkEnableOption "nodejs";
  };

  config = mkIf cfg.enable {
    home.file.".npmrc".text = ''
      prefix=${config.home.homeDirectory}/.local/share/npm
      cache=${config.home.homeDirectory}/.local/share/npm/cache
      init-module=${config.home.homeDirectory}/.local/share/npm/npm-init.js
    '';
  };
}