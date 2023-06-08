{ config, pkgs, lib, ... }:

let
  stdenv = pkgs.stdenv;
in {
  home.username = "ccruz";
  home.homeDirectory = if stdenv.isDarwin then "/Users/ccruz" else "/home/ccruz";
  home.stateVersion = "23.11";
  home.packages = [
    pkgs.bat
    pkgs.bat-extras.batdiff
    pkgs.bat-extras.batgrep
    pkgs.bat-extras.batman
    pkgs.bat-extras.batwatch
    pkgs.exa
    pkgs.mosh
    pkgs.neovim
    pkgs.pastel
    pkgs.rcm
    pkgs.rlwrap
  ];
  home.sessionVariables = {
    LESSHISTFILE = "$HOME/.cache/lesshst";
    NODE_REPL_HISTORY = "$HOME/.cache/node_repl_history";
  };

  programs.home-manager.enable = true;

  imports = [
    ./modules/home-manager/bash.nix
    ./modules/home-manager/fish.nix
    ./modules/home-manager/nodejs.nix
    ./modules/home-manager/starship.nix
    ./modules/home-manager/terminfo.nix
    ./modules/home-manager/tmux.nix
    ./programs/vim/vim.nix
    ./programs/zsh/zsh.nix
  ];

  # Settings
  modules.bash.enable = true;
  modules.fish = {
    enable = true;
    plugins = [
      {# 02/18/2021
        name = "bass";
        src = pkgs.fetchFromGitHub {
          owner = "edc";
          repo = "bass";
          rev = "2fd3d2157d5271ca3575b13daec975ca4c10577a";
          sha256 = "fl4/Pgtkojk5AE52wpGDnuLajQxHoVqyphE90IIPYFU=";
        };
      }
    ];
    functions = {
      bb = ''
        if test -z "$argv"; command rlwrap bb; else; command bb $argv; end
      '';
      emacs = ''
        TERM=xterm-24bits command emacs -nw $argv
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
      npm = ''
        switch $argv[1]
          case packages
            command npm list --depth 0 $argv[2..-1]
          case '*'
            command npm $argv
        end
      '';
    };
    aliases = {
      cat = "bat -p";
      diff = "batdiff";
      doom = "~/.emacs.d/bin/doom";
      g = "git";
      ll = "TZ=UTC exa -aghl --group-directories-first";
      ls = "TZ=UTC exa";
      lt = "TZ=UTC exa --long --tree";
      man = "batman";
      mkex = "chmod u+x";
      nodejs = "node";
      t = "tmux";
      tree = "exa --tree";
      untar = "tar -xzf";
      vim = "nvim";
      watch = "batwatch";
    };
  };
  modules.nodejs.enable = true;
  modules.starship.enable = true;
  modules.terminfo.enable = true;
  modules.tmux.enable = true;
}
