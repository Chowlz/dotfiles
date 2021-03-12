{ config, pkgs, lib, ... }:

let
  stdenv = pkgs.stdenv;
in {
  home.username = "ccruz";
  home.homeDirectory = if stdenv.isDarwin then "/Users/ccruz" else "/home/ccruz";
  home.stateVersion = "21.03";
  home.packages = [
    pkgs.awscli
    pkgs.babashka
    pkgs.bat
    pkgs.bat-extras.batdiff
    pkgs.bat-extras.batgrep
    pkgs.bat-extras.batman
    pkgs.bat-extras.batwatch
    pkgs.clj-kondo
    pkgs.clojure
    pkgs.diceware
    pkgs.exa
    pkgs.lastpass-cli
    pkgs.neofetch
    pkgs.neovim
    pkgs.pastel
    pkgs.python39
    pkgs.python39Packages.pip
    pkgs.rcm
    pkgs.ripgrep
    pkgs.rlwrap
    pkgs.shellcheck
  ] ++ lib.optionals stdenv.isDarwin [
    pkgs.coreutils
    pkgs.curl
    pkgs.diffutils
    pkgs.findutils
    pkgs.gawk
    pkgs.git
    pkgs.gnugrep
    pkgs.gnused
    pkgs.gnutar
    pkgs.gzip
    pkgs.ispell
    pkgs.keychain
    pkgs.lzma
    pkgs.mosh
    pkgs.openssh
    pkgs.tmux
    pkgs.wget
    pkgs.which
    pkgs.zsh
    pkgs.zsh-powerlevel10k
  ];

  programs.home-manager.enable = true;

  imports = [
    ./modules/fish.nix
    ./modules/nodejs.nix
    ./modules/terminfo.nix
    ./modules/tmux.nix
    ./programs/vim/vim.nix
    ./programs/zsh/zsh.nix
  ];

  # Settings
  modules.fish = {
    enable = true;
    package = pkgs.fish;
    plugins = [
      {# 02/18/2021
        name = "bass";
        src = pkgs.fetchFromGitHub {
          owner = "edc";
          repo = "bass";
          rev = "d63054b24c2f63aaa3a08fb9ec9d0da4c70ab922";
          sha256 = "0pwci5xxm8308nrb52s5nyxijk0svar8nqrdfvkk2y34z1cg319b";
        };
      }
      { # 03/04/2021
        name = "tide";
        src = fetchTarball {
          url = "https://codeload.github.com/IlanCosman/tide/tar.gz/v4.2.0";
          sha256 = "039bzhz0ii5n2iyiw4vqxgf15m1bz51np4gy5f698wyfj2b03m83";
        };
      }
    ];
    functions = {
      bb = ''
        if test -z "$argv"; command rlwrap bb; else; command bb $argv; end
      '';
      emacs = ''
        TERM=xterm-24bits command emacs -nw "$@"
      '';
      env-vars = ''
        printenv | sort | bat --pager cat -pl ini
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
      untar = "tar -xzf";
      vim = "nvim";
      watch = "batwatch";
    };
  };
  modules.nodejs.enable = true;
  modules.nodejs.package = pkgs.nodejs;
  modules.terminfo.enable = true;
  modules.tmux.enable = true;
  modules.tmux.package= pkgs.tmux;
}
