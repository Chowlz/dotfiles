{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.tmux;

  boolToStr = value: if value then "on" else "off";
in {
  options.modules.tmux = {
    enable = mkEnableOption "tmux";

    package = mkOption {
      type = types.nullOr types.package;
      default = null;
      defaultText = literalExample "pkgs.tmux";
      example = literalExample "pkgs.tmux";
      description = "The tmux package to install. May be used to change the version." ;
    };

    aggressiveResize = mkOption {
      default = true;
      type = types.bool;
      description = ''
        Resize the window to the size of the smallest session for which it is
        the current window.
      '';
    };

    baseIndex = mkOption {
      default = 0;
      example = 1;
      type = types.ints.unsigned;
      description = "Base index for panes and windows.";
    };

    clock24 = mkOption {
      default = false;
      type = types.bool;
      description = "Use 24 hour clock.";
    };

    escapeTime = mkOption {
      default = 0;
      example = 0;
      type = types.ints.unsigned;
      description = ''
        Time in milliseconds for which tmux waits after an escape is
        input.
     '';
    };

    historyLimit = mkOption {
      default = 10000;
      example = 5000;
      type = types.ints.positive;
      description = "Maximum number of lines held in window history.";
    };

    keyMode = mkOption {
      default = "emacs";
      example = "vi";
      type = types.enum [ "emacs" "vi" ];
      description = "VI or Emacs style shortcuts.";
    };

    prefix = mkOption {
      default = "C-a";
      example = "C-a";
      type = types.nullOr types.str;
      description = "Set the prefix";
    };

    shell = mkOption {
      default = null;
      example = "\${pkgs.zsh}/bin/zsh";
      type = with types; nullOr str;
      description = "Set the default-shell tmux variable.";
    };

    terminal = mkOption {
      default = "xterm-256color";
      example = "xterm-256color";
      type = types.str;
      description = "Set the $TERM variable.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = if (cfg.package != null) then [ cfg.package ] else [ ];
    home.file.".config/tmux/tmux.conf".text = ''
      ################################################################################
      # ~/.config/tmux/tmux.conf
      # Managed by nix
      ################################################################################

      '' +
      optionalString (cfg.shell != null) ''
        # Shell
        set -g default-shell ${cfg.shell}

      '' +
      optionalString (cfg.prefix != null) ''
        # Prefix
        set -g prefix ${cfg.prefix}
        unbind C-b
        # Allow moving to front of line with repeated presses
        bind a send-prefix
        bind C-a send-prefix

      '' +
      ''
      # General config
      set -g status-keys ${cfg.keyMode}
      set -g mode-keys ${cfg.keyMode}
      set -sg escape-time ${toString cfg.escapeTime}
      set -g history-limit ${toString cfg.historyLimit}
      set -g base-index ${toString cfg.baseIndex}
      setw -g pane-base-index ${toString cfg.baseIndex}
      setw -g aggressive-resize ${boolToStr cfg.aggressiveResize}
      setw -g window-size "smallest"
      setw -g clock-mode-style ${if cfg.clock24 then "24" else "12"}

      # Command aliases
      set -s command-alias[20] move="move-window -t"
      set -s command-alias[21] swap="swap-window -t"
      set -s command-alias[22] rename="rename-window"
      set -s command-alias[23] kill="kill-window"

      # Keybindings
      bind \\ split-window -h
      bind - split-window -v
      bind & kill-window
      bind x kill-pane

      # Colors
      set -g default-terminal ${cfg.terminal}
      '' +
      optionalString (cfg.terminal == "xterm-256color") ''
        set -ga terminal-overrides ",*256color*:Tc"
      '';
  };
}