{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.modules.git;
in {
  options.modules.git = {
    enable = mkEnableOption "git";

    package = mkOption {
      type = types.nullOr types.package;
      default = pkgs.git;
      defaultText = literalExample "pkgs.git";
      example = literalExample "pkgs.git";
      description = "The git package to install. May be used to change the version." ;
    };

    user = {
      email = mkOption {
        type = types.str;
        default = "";
        example = "foo@bar.com";
        description = "User's email";
      };
      name = mkOption {
        type = types.str;
        default = "";
        example = "Charles Cruz";
        description = "User's name";
      };
    };

    wsl-ssh-1password = mkOption {
      default = false;
      type = types.bool;
      description = "Use 1Password ssh-agent in wsl";
    };
  };

  config = mkIf cfg.enable {
    home.packages = if (cfg.package != null) then [ cfg.package ] else [ ];

    xdg.configFile."git/config".text = ''
      ################################################################################
      # ~/.config/git/config
      # Managed by nix
      ################################################################################
      '' +
      optionalString (cfg.user.email != "" || cfg.user.name != "") ''
        [user]
      '' +
      optionalString (cfg.user.email != "") ''
        email = ${cfg.user.email}
      '' +
      optionalString (cfg.user.name != "") ''
        name = ${cfg.user.name}
      '' +
      ''
      [pretty]
      lol = "%C(yellow bold)%h%C(reset) %C(red)(%an)%C(reset)%C(blue bold)%d%C(reset) %s %C(cyan)(%ar)%C(reset)"
      lolol = "%C(yellow bold)%h%Creset by %C(red)%an%Creset (%ar)%C(cyan bold)%d%Creset%n%s%n%n%b"
      [core]
      autocrlf = input
      editor = nvim
      '' +
      optionalString (cfg.wsl-ssh-1password == true) ''
        sshCommand = ssh.exe
      '' +
      ''
      [push]
      default = upstream
      [pull]
      rebase = true
      [rebase]
      autoStash = true
      [alias]
      pl = pull
      plr = pull --rebase
      ph = push
      co = checkout
      cob = checkout -b
      com = checkout master
      cp = cherry-pick
      ci = commit -v
      st = status
      fe = fetch
      br = branch
      rb = rebase
      rbi = rebase -i
      rbu = rebase @{upstream}
      set-upstream = !sh -c 'git branch --set-upstream-to="$0"/"$1" "$(git rev-parse --abbrev-ref HEAD)"'
      undo-commit = !sh -c 'git reset --soft HEAD~"''${1:-1}"'
      kill-commit = !sh -c 'git reset --hard HEAD~"''${1:-1}"'
      unstage = reset HEAD --
      cdiff = diff --color-words
      diffc = diff --cached
      cdiffc = diff --color-words --cached
      cshow = show --color-words
      tree = log --pretty=format:'%h %s' --graph
      apple-pick = !sh -c 'git rev-list --reverse "$@" | xargs -n1 git cherry-pick' -
      clog = log --color-words
      last = log -1 HEAD
      lol = log --graph --decorate --oneline
      lola = log --graph --decorate --pretty=lol --all
      lolo = log --graph --decorate --pretty=lol @{upstream}..HEAD
      lolu = log --graph --decorate --pretty=lol HEAD..@{upstream}
      loly = !git lol ^$(git merge-base HEAD @{upstream})^@ HEAD @{upstream}
    '';
  };
}