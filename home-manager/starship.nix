{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.modules.starship;
  languages = [
    { symbol = ""; name = "golang"; }
    { symbol = "󱃾"; name = "helm"; }
    { symbol = ""; name = "java"; }
    { symbol = ""; name = "nodejs"; }
    { symbol = ""; name = "python"; }
    { symbol = ""; name = "rust"; }
  ];
in {
  options.modules.starship = {
    enable = mkEnableOption "starship";

    package = mkOption {
      type = types.nullOr types.package;
      default = pkgs.starship;
      defaultText = literalExample "pkgs.starship";
      example = literalExample "pkgs.starship";
      description = "The starship package to install. May be used to change the version." ;
    };
  };

  config = mkIf cfg.enable {
    home.packages = if (cfg.package != null) then [ cfg.package ] else [ ];

    xdg.configFile."starship.toml".text = ''
      ################################################################################
      # ~/.config/starship.toml
      # Managed by nix
      ################################################################################
      command_timeout = 1000
      format = """
      [░▒▓](#a3aed2)\
      $nix_shell\
      [ $username$hostname ](fg:#090c0c bg:#a3aed2)\
      [](fg:#a3aed2 bg:#769ff0)\
      $directory\
      [](fg:#769ff0 bg:#394260)\
      $git_branch\
      $git_status\
      [](fg:#394260 bg:#212736)\
      '' +
      lib.strings.concatStrings (builtins.map (x: "$" + "${x.name}" + "\\\n") languages) +
      ''
      [](fg:#212736 bg:#1d2230)\
      $time\
      [ ](fg:#1d2230)\
      \n$character
      """

      [hostname]
      ssh_only = true
      style = "bold bg:#a3aed2"
      format = '[@$hostname]($style)'

      [directory]
      style = "bold fg:#e3e5e5 bg:#769ff0"
      format = "[ $path ]($style)"
      truncation_length = 3
      truncation_symbol = "…/"

      [directory.substitutions]
      "Documents" = "󰈙 "
      "Downloads" = " "
      "Music" = " "
      "Pictures" = " "

      [git_branch]
      symbol = ""
      style = "bg:#394260"
      format = '[[ $symbol $branch ](fg:#769ff0 bg:#394260)]($style)'

      [git_status]
      style = "bg:#394260"
      format = '[[($all_status$ahead_behind )](fg:#769ff0 bg:#394260)]($style)'

      [time]
      disabled = false
      time_format = "%R" # Hour:Minute Format
      style = "bg:#1d2230"
      format = '[[  $time ](bold fg:#a0a9cb bg:#1d2230)]($style)'

      [username]
      show_always = true
      style_user = "bold bg:#a3aed2"
      style_root = "bold bg:#a3aed2"
      format = '[$user]($style)'
      disabled = false

      # Languages
      '' +
      lib.strings.concatStrings (builtins.map (x:
        ''

        [${x.name}]
        symbol = "${x.symbol}"
        style = "bold bg:#212736"
        format = '[[ $symbol ($version) ](fg:#769ff0 bg:#212736)]($style)'
        ''
        ) languages) +
      ''

      # Environments

      [aws]
      symbol = ""
      style = "bold bg:#a3aed2"
      format = '[ $symbol($profile )(\($region\) )]($style)'

      [nix_shell]
      symbol = "󱄅"
      style = "bold bg:#a3aed2"
      impure_msg = '[](bold bg:#a3aed2 fg:red)'
      pure_msg = '[](bold bg:#a3aed2 fg:green)'
      unknown_msg = '[](bold bg:#a3aed2 fg:yellow)'
      format = '[ $symbol $state( \($name\))]($style)'
      '';
  };
}