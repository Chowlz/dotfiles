{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.modules.nodejs;
in {
  options.modules.nodejs = {
    enable = mkEnableOption "nodejs";

    package = mkOption {
      type = types.nullOr types.package;
      default = null;
      defaultText = literalExample "pkgs.nodejs";
      example = literalExample "pkgs.nodejs";
      description = "The nodejs package to install. May be used to change the version." ;
    };
  };

  config = mkIf cfg.enable {
    home.packages = if (cfg.package != null) then [ cfg.package ] else [ ];

    home.file.".npmrc".text = ''
      prefix=${config.home.homeDirectory}/.local/share/npm
      cache=${config.home.homeDirectory}/.local/share/npm/cache
      init-module=${config.home.homeDirectory}/.local/share/npm/npm-init.js
    '';
  };
}
