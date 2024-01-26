{ pkgs, ... }:
{
  nix = {
    settings = {
      accept-flake-config = true;
      auto-optimise-store = true;
      experimental-features = [ "nix-command" "flakes" ];
    };

    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
    };
  };
}