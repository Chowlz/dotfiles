{ config, lib, pkgs, ... }:

let
  unstable = import <nixpkgs-unstable> {};
in
{
  programs.fish = {
    enable = true;
    package = unstable.fish;
    plugins = [
      {
        # 10/21/2020
        name = "theme-bobthefish";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "theme-bobthefish";
          rev = "57d172882f61a4d896ed584cc7e7bf1bd13a08ea";
          sha256 = "04j9rzyf9va77jgm8pbjzs4mmzpy311j99gnggq09y5vc9zhv3pc";
        };
      }
      {
        # 08/23/2020
        name = "z";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "z";
          rev = "78861a85fc4da704cd7d669c1133355c89a4c667";
          sha256 = "1ffjihdjbj3359hjhg2qw2gfx5h7rljlz811ma0a318nkdcg1asx";
        };
      }
    ];
    functions = {
      # npm = ''
      #   switch $argv[1]
      #     case packages
      #       command npm list --depth 0 $argv[2..-1]
      #     case *
      #       command npm $argv
      #   end
      # '';
      bb = "if test -z $argv; command rlwrap bb; else; command bb $argv; end";
      paths = "for path in $PATH; echo $path; end";
    };
    shellAliases = {
      ".." = "cd ..";
      cat = "bat -p";
      diff = "batdiff";
      doom = "~/.emacs.d/bin/doom";
      emacs = "TERM=xterm-24bits command emacs -nw";
      # env-vars = "env | bat -pl ini";
      g = "git";
      # grep = "batgrep"; # https://github.com/eth-p/bat-extras/issues/42
      grep = "rg";
      ll = "TZ=UTC exa -aghl";
      ls = "TZ=UTC exa";
      lt = "TZ=UTC exa --long --tree";
      man = "batman";
      mkex = "chmod u+x";
      nodejs = "node";
      t = "tmux";
      untar = "tar -xzf";
      vim = "nvim";
      watch = "batwatch";
    };
    shellInit = ''
      set -g theme_color_scheme dracula
    '';
  };

  xdg.configFile."fish/conf.d/plugin-theme-bobthefish.fish".text = lib.mkAfter ''
    for f in $plugin_dir/*.fish
      source $f
    end
  '';
}
