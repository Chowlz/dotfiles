{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.yabai;

  configModule = types.submodule {
    options = {
      debugOutput = mkOption {
        type = types.nullOr (types.enum ["on" "off"]);
        default = null;
        description = ''
          Enable output of debug information to stdout.
        '';
      };

      externalBar = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          <main|all|off>:<top_padding>:<bottom_padding>
          Specify top and bottom padding for a potential custom bar that you may be running.
          main: Apply the given padding only to spaces located on the main display.
          all: Apply the given padding to all spaces regardless of their display.
          off: Do not apply any special padding.
        '';
      };

      mouseFollowsFocus = mkOption {
        type = types.nullOr (types.enum ["on" "off"]);
        default = null;
        description = ''
          When focusing a window, put the mouse at its center.
        '';
      };

      focusFollowsMouse = mkOption {
        type = types.nullOr (types.enum ["autofocus" "autoraise" "off"]);
        default = null;
        description = ''
          Automatically focus the window under the mouse.
        '';
      };

      windowPlacement = mkOption {
        type = types.nullOr (types.enum ["first_child" "second_child"]);
        default = null;
        description = ''
          Specify whether managed windows should become the first or second leaf-node.
        '';
      };

      windowTopmost = mkOption {
        type = types.nullOr (types.enum ["on" "off"]);
        default = null;
        description = ''
          Make floating windows stay on top.
        '';
      };

      windowShadow = mkOption {
        type = types.nullOr (types.either (types.enum [null "on" "off"]) types.float);
        default = null;
        description = ''
          Draw shadow for windows.
        '';
      };

      windowOpacity = mkOption {
        type = types.nullOr (types.enum ["on" "off"]);
        default = null;
        description = ''
          Enable opacity for windows.
        '';
      };

      windowOpacityDuration = mkOption {
        type = types.nullOr types.float;
        default = null;
        description = ''
          Duration of transition between active / normal opacity.
        '';
      };

      activeWindowOpacity = mkOption {
        type = types.nullOr types.float;
        default = null;
        description = ''
          0 < <value> <= 1.0
          Opacity of the focused window.
        '';
      };

      normalWindowOpacity = mkOption {
        type = types.nullOr types.float;
        default = null;
        description = ''
          0 < <value> <= 1.0
          Opacity of an unfocused window.
        '';
      };

      windowBorder = mkOption {
        type = types.nullOr (types.enum ["on" "off"]);
        default = null;
        description = ''
          Draw border for windows.
        '';
      };

      windowBorderWidth = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = ''
          <even integer number>
          Width of window borders. If the given width is an odd number, it will be incremented by 1.
        '';
      };

      activeWindowBorderColor = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "0xAARRGGBB";
        description = ''
          Color of the border of the focused window.
        '';
      };

      normalWindowBorderColor = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "0xAARRGGBB";
        description = ''
          Color of the border of an unfocused window.
        '';
      };

      insertFeedbackColor = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "0xAARRGGBB";
        description = ''
          Color of the window --insert message selection.
        '';
      };

      splitRatio = mkOption {
        type = types.nullOr types.float;
        default = null;
        description = ''
          0 < <value> <= 1.0
          Default split ratio.
        '';
      };

      autoBalance = mkOption {
        type = types.nullOr (types.enum ["on" "off"]);
        default = null;
        description = ''
          Balance the window tree upon change, so that all windows occupy the same area.
        '';
      };

      mouseModifier = mkOption {
        type = types.nullOr (types.enum ["cmd" "alt" "shift" "ctrl" "fn"]);
        default = null;
        description = ''
          Keyboard modifier used for moving and resizing windows.
        '';
      };

      mouseAction1 = mkOption {
        type = types.nullOr (types.enum ["move" "resize"]);
        default = null;
        description = ''
          Action performed when pressing mouse_modifier + button1.
        '';
      };

      mouseAction2 = mkOption {
        type = types.nullOr (types.enum ["move" "resize"]);
        default = null;
        description = ''
          Action performed when pressing mouse_modifier + button2.
        '';
      };

      mouseDropAction = mkOption {
        type = types.nullOr (types.enum ["swap" "stack"]);
        default = null;
        description = ''
          Action performed when a bsp-managed window is dropped in the center of some other
          bsp-managed window.
        '';
      };

      layout = mkOption {
        type = types.nullOr (types.enum ["bsp" "stack" "float"]);
        default = null;
        description = ''
          Set the layout of the selected space.
        '';
      };

      topPadding = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = ''
          Padding added at the upper side of the selected space.
        '';
      };

      bottomPadding = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = ''
          Padding added at the lower side of the selected space.
        '';
      };

      leftPadding = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = ''
          Padding added at the left side of the selected space.
        '';
      };

      rightPadding = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = ''
          Padding added at the right side of the selected space.
        '';
      };

      windowGap = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = ''
          Size of the gap that separates windows for the selected space.
        '';
      };
    };
  };

  regexModule = types.submodule {
    options = {
      invert = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Inverts regex.
        '';
      };
      regex = mkOption {
        type = types.str;
        description = ''
          Regex.
        '';
      };
    };
  };

  ruleModule = types.submodule {
    options = {
      label = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Label used to identify the rule with a unique name.
        '';
      };
      app = mkOption {
        type = types.nullOr regexModule;
        default = null;
        description = ''
          Name of application. Use { invert: false } to invert the match.
        '';
      };
      title = mkOption {
        type = types.nullOr regexModule;
        default = null;
        description = ''
          Title of window. Use { invert: false } to invert the match.
        '';
      };
      display = mkOption {
        type = types.nullOr (types.either
          (types.enum ["prev" "next" "first" "last" "recent" "mouse" "north" "east" "south" "west"])
          types.int);
        default = null;
        description = ''
          Send window to display. If ^ is present, follow focus.
        '';
      };

      space = mkOption {
        type = types.nullOr (types.either
          (types.enum ["prev" "next" "first" "last" "recent" "mouse"])
          (types.either types.int types.str));
        default = null;
        description = ''
          Send window to space. If ^ is present, follow focus.
        '';
      };

      manage = mkOption {
        type = types.nullOr (types.enum ["on" "off"]);
        default = null;
        description = ''
          Window should be managed (tile vs float).
        '';
      };

      sticky = mkOption {
        type = types.nullOr (types.enum ["on" "off"]);
        default = null;
        description = ''
          Window appears on all spaces.
        '';
      };

      mouseFollowsFocus = mkOption {
        type = types.nullOr (types.enum ["on" "off"]);
        default = null;
        description = ''
          When focusing the window, put the mouse at its center. Overrides the global
          mouseFollowsFocus setting.
        '';
      };

      layer = mkOption {
        type = types.nullOr (types.enum ["below" "normal" "above"]);
        default = null;
        description = ''
          Window is ordered within the given stacking layer.
        '';
      };

      opacity = mkOption {
        type = types.nullOr types.float;
        default = null;
        description = ''
          0 < <value> <= 1.0
          Set window opacity. The window will no longer be eligible for automatic change in opacity
          upon focus change.
        '';
      };

      border = mkOption {
        type = types.nullOr (types.enum ["on" "off"]);
        default = null;
        description = ''
          Window should draw border.
        '';
      };

      nativeFullscreen = mkOption {
        type = types.nullOr (types.enum ["on" "off"]);
        default = null;
        description = ''
          Window should enter native macOS fullscreen mode.
        '';
      };

      grid = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          <rows>:<cols>:<start-x>:<start-y>:<width>:<height>
          Set window frame based on a self-defined grid.
        '';
      };
    };
  };

  mkConfigLine = k: v:
    if v == null then [] else ["yabai -m config ${k} ${toString v}"];

  mkRuleArg = k: v:
    if v == null
    then []
      else
        (if (isAttrs v)
          then ["${if v.invert then "!" else ""}${k}=\"${v.regex}\""]
          else ["${k}=${toString v}"]);

  yabairc =
    let
      config = optionalString (cfg.config != null) (''
          # Config
        '' + (concatStringsSep "\n"
          ((mkConfigLine "debug_output" cfg.config.debugOutput) ++
          (mkConfigLine "external_bar" cfg.config.externalBar) ++
          (mkConfigLine "mouse_follows_focus" cfg.config.externalBar) ++
          (mkConfigLine "focus_follows_mouse" cfg.config.focusFollowsMouse) ++
          (mkConfigLine "window_placement" cfg.config.windowPlacement) ++
          (mkConfigLine "window_topmost" cfg.config.windowTopmost) ++
          (mkConfigLine "window_shadow" cfg.config.windowShadow) ++
          (mkConfigLine "window_opacity" cfg.config.windowOpacity) ++
          (mkConfigLine "window_opacity_duration" cfg.config.windowOpacityDuration) ++
          (mkConfigLine "active_window_opacity" cfg.config.activeWindowOpacity) ++
          (mkConfigLine "normal_window_opacity" cfg.config.normalWindowOpacity) ++
          (mkConfigLine "window_border" cfg.config.windowBorder) ++
          (mkConfigLine "window_border_width" cfg.config.windowBorderWidth) ++
          (mkConfigLine "active_window_border_color" cfg.config.activeWindowBorderColor) ++
          (mkConfigLine "normal_window_border_color" cfg.config.normalWindowBorderColor) ++
          (mkConfigLine "insert_feedback_color" cfg.config.insertFeedbackColor) ++
          (mkConfigLine "split_ratio" cfg.config.splitRatio) ++
          (mkConfigLine "auto_balance" cfg.config.autoBalance) ++
          (mkConfigLine "mouse_modifier" cfg.config.mouseModifier) ++
          (mkConfigLine "mouse_action1" cfg.config.mouseAction1) ++
          (mkConfigLine "mouse_action2" cfg.config.mouseAction2) ++
          (mkConfigLine "mouse_drop_action" cfg.config.mouseDropAction) ++
          (mkConfigLine "layout" cfg.config.layout) ++
          (mkConfigLine "top_gap" cfg.config.topPadding) ++
          (mkConfigLine "bottom_gap" cfg.config.bottomPadding) ++
          (mkConfigLine "left_gap" cfg.config.leftPadding) ++
          (mkConfigLine "right_gap" cfg.config.rightPadding) ++
          (mkConfigLine "window_gap" cfg.config.windowGap))) +
        "\n");
      rules = optionalString (cfg.rules != null) (''
          # Rules
        '' + (concatStringsSep "\n"
          (map (rule:
            "yabai -m rule --add " +
            (concatStringsSep " " (
              (mkRuleArg "label" rule.label) ++
              (mkRuleArg "app" rule.app) ++
              (mkRuleArg "title" rule.title) ++
              (mkRuleArg "display" rule.display) ++
              (mkRuleArg "space" rule.space) ++
              (mkRuleArg "manage" rule.manage) ++
              (mkRuleArg "sticky" rule.sticky) ++
              (mkRuleArg "mouse_follows_focus" rule.mouseFollowsFocus) ++
              (mkRuleArg "layer" rule.layer) ++
              (mkRuleArg "opacity" rule.opacity) ++
              (mkRuleArg "border" rule.border) ++
              (mkRuleArg "native-fullscreen" rule.nativeFullscreen) ++
              (mkRuleArg "grid" rule.grid))))
            cfg.rules)) +
        "\n");
      rc = config + rules;
    in
      optionalString (rc != "") (''
          ################################################################################
          # yabairc
          ################################################################################
        '' +
        rc);

  configFile = mkIf (yabairc != "")
    "${pkgs.writeScript "yabairc" yabairc}";
in {
  options.modules.yabai = {
    enable = mkEnableOption "yabai, a window manager";

    package = mkOption {
      type = types.package;
      default = pkgs.yabai;
      defaultText = literalExample "pkgs.yabai";
      description = ''
        The yabai package to install. May be used to change the version.
      '';
    };

    config = mkOption {
      type = types.nullOr configModule;
      default = null;
      description = ''
        Key/Value pairs to pass to yabai's config.
      '';
    };

    rules = mkOption {
      type = types.nullOr (types.listOf ruleModule);
      default = null;
      description = ''
        Key/Value pairs to pass to yabai's rules
      '';
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    launchd.user.agents.yabai = {
      serviceConfig.ProgramArguments =
        [ "${cfg.package}/bin/yabai" ] ++
        (optionals (yabairc != "") [ "-c" configFile ]);
      serviceConfig.KeepAlive = true;
      serviceConfig.RunAtLoad = true;
      serviceConfig.EnvironmentVariables = {
        PATH = "${cfg.package}/bin:${config.environment.systemPath}";
      };
    };
  };
}
