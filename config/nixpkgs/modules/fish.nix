{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.fish;

  pluginModule = types.submodule ({ config, ... }: {
    options = {
      src = mkOption {
        type = types.path;
        description = ''
          Path to the plugin folder.
          </para><para>
          Relevant pieces will be added to the fish function path and
          the completion path. The <filename>init.fish</filename> and
          <filename>key_binding.fish</filename> files are sourced if
          they exist.
        '';
      };

      name = mkOption {
        type = types.str;
        description = ''
          The name of the plugin.
        '';
      };
    };
  });

  functionModule = types.submodule {
    options = {
      body = mkOption {
        type = types.lines;
        description = ''
          The function body.
        '';
      };

      argumentNames = mkOption {
        type = with types; nullOr (either str (listOf str));
        default = null;
        description = ''
          Assigns the value of successive command line arguments to the names
          given.
        '';
      };

      description = mkOption {
        type = with types; nullOr str;
        default = null;
        description = ''
          A description of what the function does, suitable as a completion
          description.
        '';
      };

      wraps = mkOption {
        type = with types; nullOr str;
        default = null;
        description = ''
          Causes the function to inherit completions from the given wrapped
          command.
        '';
      };

      onEvent = mkOption {
        type = with types; nullOr str;
        default = null;
        description = ''
          Tells fish to run this function when the specified named event is
          emitted. Fish internally generates named events e.g. when showing the
          prompt.
        '';
      };

      onVariable = mkOption {
        type = with types; nullOr str;
        default = null;
        description = ''
          Tells fish to run this function when the specified variable changes
          value.
        '';
      };

      onJobExit = mkOption {
        type = with types; nullOr (either str int);
        default = null;
        description = ''
          Tells fish to run this function when the job with the specified group
          ID exits. Instead of a PID, the stringer <literal>caller</literal> can
          be specified. This is only legal when in a command substitution, and
          will result in the handler being triggered by the exit of the job
          which created this command substitution.
        '';
      };

      onProcessExit = mkOption {
        type = with types; nullOr (either str int);
        default = null;
        example = "$fish_pid";
        description = ''
          Tells fish to run this function when the fish child process with the
          specified process ID exits. Instead of a PID, for backwards
          compatibility, <literal>%self</literal> can be specified as an alias
          for <literal>$fish_pid</literal>, and the function will be run when
          the current fish instance exits.
        '';
      };

      onSignal = mkOption {
        type = with types; nullOr (either str int);
        default = null;
        example = [ "SIGHUP" "HUP" 1 ];
        description = ''
          Tells fish to run this function when the specified signal is
          delievered. The signal can be a signal number or signal name.
        '';
      };

      noScopeShadowing = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Allows the function to access the variables of calling functions.
        '';
      };

      inheritVariable = mkOption {
        type = with types; nullOr str;
        default = null;
        description = ''
          Snapshots the value of the specified variable and defines a local
          variable with that same name and value when the function is defined.
        '';
      };
    };
  };

  indentStrList= {break ? true, header ? null, prefix, list}:
    (if header != null
      then "${prefix}# ${header}\n${prefix}"
      else "${prefix}") +
    (concatStringsSep "\n${prefix}" list) +
    (optionalString break "\n");

  indentStrBlock = {break ? true, header ? null, prefix, str}:
    optionalString (str != "") (indentStrList {
      inherit break header prefix;
      list=(splitString "\n" (removeSuffix "\n" str));
    });

  indentAttrsOfStrBlock = {break ? true, header ? null, prefix, func, attrs}:
    optionalString (attrs != {}) (indentStrList {
      inherit break header prefix;
      list=(mapAttrsToList func attrs);
    });

  ifStrBlock = {header ? null, cond, str}:
    optionalString
      (str != "")
      ((optionalString
        (header != null)
        "# ${header}\n") + "if ${cond}\n" + str + "end\n");
in {
  options.modules.fish = {
    enable = mkEnableOption "fish, the friendly interactive shell";

    package = mkOption {
      type = types.package;
      default = pkgs.fish;
      defaultText = literalExample "pkgs.fish";
      description = ''
        The fish package to install. May be used to change the version.
      '';
    };

    abbrs = mkOption {
      type = with types; attrsOf str;
      default = {};
      example = {
        l = "less";
        gco = "git checkout";
      };
      description = ''
        An attribute set that maps aliases (the top level attribute names
        in this option) to abbreviations. Abbreviations are expanded with
        the longer phrase after they are entered.
      '';
    };

    aliases = mkOption {
      type = with types; attrsOf str;
      default = { };
      example = literalExample ''
        {
          ll = "ls -l";
          ".." = "cd ..";
        }
      '';
      description = ''
        An attribute set that maps aliases (the top level attribute names
        in this option) to command strings or directly to build outputs.
      '';
    };

    interactiveShellInit = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Shell script code called during interactive fish shell init.
      '';
    };

    loginShellInit = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Shell script code called during fish login shell init.
      '';
    };

    functions = mkOption {
      type = with types; attrsOf (either lines functionModule);
      default = { };
      example = literalExample ''
        {
          __fish_command_not_found_handler = {
            body = "__fish_default_command_not_found_handler $argv[1]";
            onEvent = "fish_command_not_found";
          };
          gitignore = "curl -sL https://www.gitignore.io/api/$argv";
        }
      '';
      description = ''
        Basic functions to add to fish. For more information see
        <link xlink:href="https://fishshell.com/docs/current/cmds/function.html"/>.
      '';
    };

    plugins = mkOption {
      type = types.listOf pluginModule;
      default = [ ];
      example = literalExample ''
        [
          {
            name = "z";
            src = pkgs.fetchFromGitHub {
              owner = "jethrokuan";
              repo = "z";
              rev = "ddeb28a7b6a1f0ec6dae40c636e5ca4908ad160a";
              sha256 = "0c5i7sdrsp0q3vbziqzdyqn4fmp235ax4mn4zslrswvn8g3fvdyh";
            };
          }
          # oh-my-fish plugins are stored in their own repositories, which
          # makes them simple to import into home-manager.
          {
            name = "fasd";
            src = pkgs.fetchFromGitHub {
              owner = "oh-my-fish";
              repo = "plugin-fasd";
              rev = "38a5b6b6011106092009549e52249c6d6f501fba";
              sha256 = "06v37hqy5yrv5a6ssd1p3cjd9y3hnp19d3ab7dag56fs1qmgyhbs";
            };
          }
        ]
      '';
      description = ''
        The plugins to source in
        <filename>conf.d/99plugins.fish</filename>.
      '';
    };

    shellInit = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Shell script code called during fish shell init.
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [
    { home.packages = [ cfg.package ];

      xdg.dataFile."fish/home-manager_generated_completions".source = let
        # Paths later in the list will overwrite those already linked
        destructiveSymlinkJoin =
          args_@{ name, paths, preferLocalBuild ? true, allowSubstitutes ? false, postBuild ? "", ... }:
          let
            args = removeAttrs args_ [ "name" "postBuild" ] // {
              # pass the defaults
              inherit preferLocalBuild allowSubstitutes;
            };
          in pkgs.runCommand name args ''
            mkdir -p $out
            for i in $paths; do
              if [ -z "$(find $i -prune -empty)" ]; then
                cp -srf $i/* $out
              fi
            done
            ${postBuild}
          '';

        generateCompletions = package:
          pkgs.runCommand "${package.name}-fish-completions" {
            src = package;
            nativeBuildInputs = [ pkgs.python2 ];
            buildInputs = [ cfg.package ];
            preferLocalBuild = true;
            allowSubstitutes = false;
          } ''
            mkdir -p $out
            if [ -d $src/share/man ]; then
              find $src/share/man -type f \
                | xargs python ${cfg.package}/share/fish/tools/create_manpage_completions.py --directory $out \
                > /dev/null
            fi
          '';
      in destructiveSymlinkJoin {
        name = "${config.home.username}-fish-completions";
        paths =
          let cmp = (a: b: (a.meta.priority or 0) > (b.meta.priority or 0));
          in map generateCompletions (sort cmp config.home.packages);
      };

      xdg.configFile."fish/config.fish".text = ''
        ################################################################################
        # config.fish
        ################################################################################

        '' +
        ifStrBlock {
          header="Source home.sessionVariables only once";
          cond="not set -q __HM_SESS_VARS_SOURCED";
          str=
            indentAttrsOfStrBlock {
              prefix="  ";
              func=(k: v: "set -gx ${k} ${escapeShellArg v}");
              attrs=config.home.sessionVariables;
            } +
            optionalString (config.home.sessionVariables != {}) "  set -gx __HM_SESS_VARS_SOURCED 1\n";
        } +
        ifStrBlock {
          header="Login shell init";
          cond="status --is-login";
          str=indentStrBlock {
            prefix="  ";
            str=cfg.loginShellInit;
          };
        } +
        ifStrBlock {
          header="Interactive shell init";
          cond="status --is-interactive";
          str=
            indentAttrsOfStrBlock {
              header="Abbreviations";
              prefix="  ";
              func=(k: v: "abbr --add --global -- ${k} ${escapeShellArg v}");
              attrs=cfg.abbrs;
            } +
            indentAttrsOfStrBlock {
              header="Aliases";
              prefix="  ";
              func=(k: v: "alias ${k} ${escapeShellArg v}");
              attrs=cfg.aliases;
            } +
            indentStrBlock {
              header="Init";
              prefix="  ";
              str=cfg.interactiveShellInit;
            } +
            indentStrBlock {
              header="Add completions generated by Home Manager to $fish_complete_path";
              prefix="  ";
              str=''
                begin
                  set -l joined (string join " " $fish_complete_path)
                  set -l prev_joined (string replace --regex "[^\s]*generated_completions.*" "" $joined)
                  set -l post_joined (string replace $prev_joined "" $joined)
                  set -l prev (string split " " (string trim $prev_joined))
                  set -l post (string split " " (string trim $post_joined))
                  set fish_complete_path $prev "${config.xdg.dataHome}/fish/home-manager_generated_completions" $post
                end
              '';
            };
        };
    }
    {
      xdg.configFile = mapAttrs' (name: def: {
        name = "fish/functions/${name}.fish";
        value = {
          text = let
            modifierStr = n: v: optional (v != null) ''--${n}="${toString v}"'';
            modifierStrs = n: v: optional (v != null) "--${n}=${toString v}";
            modifierBool = n: v: optional (v != null && v) "--${n}";

            mods = with def;
              modifierStr "description" description ++ modifierStr "wraps" wraps
              ++ modifierStr "on-event" onEvent
              ++ modifierStr "on-variable" onVariable
              ++ modifierStr "on-job-exit" onJobExit
              ++ modifierStr "on-process-exit" onProcessExit
              ++ modifierStr "on-signal" onSignal
              ++ modifierBool "no-scope-shadowing" noScopeShadowing
              ++ modifierStr "inherit-variable" inheritVariable
              ++ modifierStrs "argument-names" argumentNames;

            modifiers = if isAttrs def then " ${toString mods}" else "";
            body = if isAttrs def then def.body else def;
          in ''
            function ${name}${modifiers}
              ${indentStrBlock { break = false; prefix = "  "; str = body; }}
            end
          '';
        };
      }) (cfg.functions // {
        fish-mgr = ''
          argparse --stop-nonopt h/help -- $argv
          if set -q _flag_help
            _tide_help
          else if functions --query _fish_plugin_$argv[1]_$argv[2]
            _fish_plugin_$argv[1]_$argv[2]
          else
            printf '%s\n' \
              "Usage: fish-mgr [plugin] command" \
              "" \
              "Options:" \
              "  -h or --help     print this help message" \
              "" \
              "Commands:" \
              "  init   run plugin init" \
              "  clear  clear plugin settings"
            return 1
          end
        '';
      });
    }

    # Each plugin gets a corresponding conf.d/plugin-NAME.fish file to load
    # in the paths and any initialization scripts.
    (mkIf (length cfg.plugins > 0) {
      xdg.configFile = mkMerge ((map (plugin: {
        "fish/conf.d/plugin-${plugin.name}.fish".text = ''
          # Plugin ${plugin.name}
          set -l plugin_dir ${plugin.src}

          function _fish_plugin_${plugin.name}_init
            set -U __fish_plugin_${plugin.name}_init 1
            set -l plugin_dir ${plugin.src}
            for f in $plugin_dir/functions/*.fish; source $f; end
            for f in $plugin_dir/conf.d/*.fish
              source $f
              if set --local name (string replace --regex -- '.+conf\.d/([^/]+)\.fish$' '$1' $f)
                  emit {$name}_install
              end
            end
          end

          function _fish_plugin_${plugin.name}_clear
            set -l plugin_dir ${plugin.src}
            for f in $plugin_dir/conf.d/*.fish;
              if set --local name (string replace --regex -- '.+conf\.d/([^/]+)\.fish$' '$1' $f)
                  emit {$name}_uninstall
              end
            end
          end

          # Only init once if settings were applied and use fish-mgr to clear settings
          if set -q __fish_plugin_${plugin.name}_init
            if test -d $plugin_dir/functions
              set fish_function_path $fish_function_path[1] $plugin_dir/functions $fish_function_path[2..-1]
            end

            if test -d $plugin_dir/completions
              set fish_complete_path $fish_complete_path[1] $plugin_dir/completions $fish_complete_path[2..-1]
            end

            if test -d $plugin_dir/conf.d
              for f in $plugin_dir/conf.d/*.fish
                source $f
              end
            end
          else
            _fish_plugin_${plugin.name}_init
          end
        '';
      }) cfg.plugins));
    })
  ]);
}
