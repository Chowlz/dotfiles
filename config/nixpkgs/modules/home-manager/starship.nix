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
      format = """
      [](#9A348E)\
      $username\
      [](bg:#DA627D fg:#9A348E)\
      $directory\
      [](fg:#DA627D bg:#FF995E)\
      $git_branch\
      $git_status\
      [](fg:#FF995E bg:#7A88CF)\
      $aws\
      $nodejs\
      [](fg:#7A88CF bg:#33658A)\
      $time\
      $status\
      [](fg:#33658A)\
      """

      # Disable the blank line at the start of the prompt
      # add_newline = false

      # You can also replace your username with a neat symbol like  to save some space
      [username]
      show_always = true
      style_user = "bg:#9A348E"
      style_root = "bg:#9A348E"
      format = '[$user ]($style)'

      [directory]
      style = "bg:#DA627D"
      format = "[ $path ]($style)"
      truncation_length = 3
      truncation_symbol = "…/"

      # Here is how you can shorten some long paths by text replacement
      # similar to mapped_locations in Oh My Posh:
      [directory.substitutions]
      "Documents" = " "
      "Downloads" = " "
      "Music" = " "
      "Pictures" = " "
      # Keep in mind that the order matters. For example:
      # "Important Documents" = "  "
      # will not be replaced, because "Documents" was already substituted before.
      # So either put "Important Documents" before "Documents" or use the substituted version:
      # "Important  " = "  "

      [git_branch]
      symbol = ""
      style = "bg:#FF995E"
      format = '[[ $symbol $branch ](bg:#FF995E)]($style)'

      [git_status]
      style = "bg:#FF995E"
      format = '[[($all_status$ahead_behind )](bg:#FF995E)]($style)'

      [aws]
      symbol = ""
      style = "bg:#7A88CF"
      format = '[[ $symbol ($profile)(\($region\) )](bg:#7A88CF)]($style)'

      [nodejs]
      symbol = ""
      style = "bg:#7A88CF"
      format = '[[ $symbol ($version) ](bg:#7A88CF)]($style)'

      [time]
      disabled = false
      time_format = "%R" # Hour:Minute Format
      style = "bg:#33658A"
      format = '[[ ♥ $time ](bg:#33658A)]($style)'

      [status]
      disabled = false
      style = "bg:#33658A"
      format = '[[ $symbol $common_meaning ](bg:#33658A)]($style)'
    '';
  };
}
