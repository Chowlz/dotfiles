{ config, pkgs, lib, ... }:

let
  stdenv = pkgs.stdenv;
in
{
  imports = [
    ../home-manager/git.nix
    ../home-manager/neovim.nix
    ../home-manager/nodejs.nix
    ../home-manager/starship.nix
    ../home-manager/terminfo.nix
    ../home-manager/tmux.nix
  ];

  # Let home-manager manage itself
  programs.home-manager.enable = true;

  home = {
    sessionPath = [
      "$HOME/bin"
    ];
    sessionVariables = {
      LESSHISTFILE = "$HOME/.cache/lesshst";
      NODE_REPL_HISTORY = "$HOME/.cache/node_repl_history";
    };
    shellAliases = {
      cat = "bat -p";
      diff = "batdiff";
      g = "git";
      grep = "grep --color=always";
      ll = "TZ=UTC eza -aghl --group-directories-first";
      ls = "TZ=UTC eza";
      lt = "TZ=UTC eza --long --tree";
      man = "batman";
      mkex = "chmod u+x";
      nodejs = "node";
      t = "tmux";
      tree = "eza --tree";
      untar = "tar -xzf";
      vim = "nvim";
      watch = "batwatch";
    };
  };

  modules.git.enable = true;
  modules.neovim.enable = true;
  modules.nodejs.enable = true;
  modules.starship.enable = true;
  modules.terminfo.enable = true;
  modules.tmux.enable = true;

  programs.bash = {
    enable = true;
    historyControl = [ "ignoredups" ];
  };
  programs.fish = {
    enable = true;
    functions = {
      bb = ''
        if test -z "$argv"; command rlwrap bb; else; command bb $argv; end
      '';
      env-vars = ''
        printenv | sort | bat --pager cat -pl ini
      '';
      decode-jwt = {
        description = "Decode a JWT";
        body = ''
          function decode_base64
            set -l result $argv[1]
            set -l len (math (string length $result) + 1)

            if [ $len -eq 2 ]
              set -l result "$1"'=='
            else if [ $len -eq 3 ]
              set -l result "$1"'='
            end
            echo "$result" | tr '_-' '/+' | base64 -d
          end

          echo "JWT Header:"
          decode_base64 (echo -n $argv[1] | cut -d "." -f 1) | jq .
          echo "JWT Body:"
          decode_base64 (echo -n $argv[1] | cut -d "." -f 2) | \
            jq "if .exp then (.expDate = (.exp|todate)) else . end"
          functions -e decode_base64
        '';
      };
      nix-paths = ''
        string split " " $NIX_PATH
      '';
      npm = ''
        switch $argv[1]
          case packages
            command npm list --depth 0 $argv[2..-1]
          case '*'
            command npm $argv
        end
      '';
      paths = ''
        for path in $PATH; echo $path; end
      '';
    };
    interactiveShellInit = ''
      set fish_greeting
      starship init fish | source
    '';
  };
  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    history = {
      ignoreAllDups = true;
      ignorePatterns = [ "rm *" "pkill *" ];
      save = 100000;
      size = 100000;
    };
    initExtra = ''
      setopt HIST_SAVE_NO_DUPS    # Don't write duplicate entries in the history file
      setopt HIST_IGNORE_SPACE    # Ignore entries that start with a space
      setopt SH_WORD_SPLIT        # Bash-like variable expansion
      setopt rmstarsilent         # Prevent zsh prompt when doing rm -f
      setopt extendedglob         # Use extended glob patterns (for external config)
      unsetopt correct_all        # Stop ZSH corrections (because it's not smart)

      bb () {
        if [ "$#" -eq 0 ]; then
          command rlwrap bb
        else
          command bb "$@"
        fi
      }

      env-vars () {
        printenv | sort | bat --pager cat -pl ini
      }

      hash-check () {
        if [ "$#" -ne 3 ]; then
          >&2 echo "Expected 3 args (hash-check <type> <file> <hash>)"; return
        fi
        local type=$1
        local file=$2
        local this_hash=$3
        local that_hash
        case "$type" in
          md5)    type=md5sum ;;
          sha1)   type=sha1sum ;;
          sha256) type=sha256sum ;;
          *)      >&2 echo "Unexpected type (expected: md5, sha1, sha256)"; return ;;
        esac
        that_hash=$($type $file | cut -f 1 -d ' ')
        if [ "$this_hash" == "$that_hash" ]; then
          echo "Match!"
        else
          echo "Mismatch! \"$this_hash\" != \"$that_hash\""
        fi
      }

      nix-paths () {
        echo $NIX_PATH | sed -e s/\\:/\\\n/g
      }

      npm () {
        case $1 in
          packages) shift; command npm list --depth 0 "$@" ;;
          *)        command npm "$@" ;;
        esac
      }

      paths () {
        echo $PATH | sed -e s/\\:/\\\n/g
      }
    '';
    syntaxHighlighting.enable = true;
  };
}