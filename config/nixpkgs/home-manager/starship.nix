{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.modules.starship;
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
      [](#9A348E)\
      $username\
      $hostname\
      [](bg:#DA627D fg:#9A348E)\
      $directory\
      [](fg:#DA627D bg:#FCA17D)\
      $git_branch\
      $git_status\
      [](fg:#FCA17D bg:#86BBD8)\
      $c\
      $elixir\
      $elm\
      $golang\
      $gradle\
      $haskell\
      $java\
      $julia\
      $nodejs\
      $nim\
      $rust\
      $scala\
      [](fg:#86BBD8 bg:#06969A)\
      $aws\
      $docker_context\
      $nix_shell\
      [](fg:#06969A bg:#33658A)\
      $time\
      [ ](fg:#33658A)\
      """

      [aws]
      symbol = ""
      style = "bg:#06969A"
      format = '[[ $symbol ($profile)(\($region\) )](bg:#06969A)]($style)'

      [directory]
      style = "bg:#DA627D"
      format = "[ $path ]($style)"
      truncation_length = 3
      truncation_symbol = "…/"

      [directory.substitutions]
      "Documents" = "󰈙 "
      "Downloads" = " "
      "Music" = " "
      "Pictures" = " "

      [c]
      symbol = " "
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [docker_context]
      symbol = " "
      style = "bg:#06969A"
      format = '[ $symbol $context ]($style) $path'

      [elixir]
      symbol = " "
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [elm]
      symbol = " "
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [git_branch]
      symbol = ""
      style = "bg:#FCA17D"
      format = '[ $symbol $branch ]($style)'

      [git_status]
      style = "bg:#FCA17D"
      format = '[$all_status$ahead_behind ]($style)'

      [golang]
      symbol = " "
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [gradle]
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [haskell]
      symbol = " "
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [hostname]
      ssh_only = true
      style = "bg:#9A348E"
      format = '[@ $hostname]($style)'

      [java]
      symbol = " "
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [julia]
      symbol = " "
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [nim]
      symbol = "󰆥 "
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [nix_shell]
      symbol = "󱄅"
      style = "bg:#06969A"
      format = '[[ $symbol ($version) ](bg:#06969A)]($style)'

      [nodejs]
      symbol = ""
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [rust]
      symbol = ""
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [scala]
      symbol = " "
      style = "bg:#86BBD8"
      format = '[ $symbol ($version) ]($style)'

      [status]
      disabled = false
      style = "bg:#33658A"
      format = '[[ $symbol $common_meaning ](bg:#33658A)]($style)'

      [time]
      disabled = false
      time_format = "%R" # Hour:Minute Format
      style = "bg:#33658A"
      format = '[ ♥ $time ]($style)'

      [username]
      show_always = true
      style_user = "bg:#9A348E"
      style_root = "bg:#9A348E"
      format = '[$user ]($style)'
      disabled = false
    '';
  };
}