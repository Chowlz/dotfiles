{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.bash;
in {
  options.modules.bash = {
    enable = mkEnableOption "bash";

    package = mkOption {
      type = types.package;
      default = pkgs.bash;
      defaultText = literalExample "pkgs.bash";
      example = literalExample "pkgs.bash";
      description = "The bash package to install. May be used to change the version." ;
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    home.file.".bashrc".text = ''
      ################################################################################
      # .bashrc
      ################################################################################

      ################################################################################
      # General config
      ################################################################################
      export HISTCONTROL=ignoredups # Ignore duplicates + commands w/ starting space
      alias ll="LC_COLLATE=C ls -alF"
      '';
  };
}
