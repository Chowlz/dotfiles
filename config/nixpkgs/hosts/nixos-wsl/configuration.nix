{ lib, pkgs, inputs, ... }:

{
  networking.hostName = "nixos-wsl";
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  programs.fish.enable = true;
  programs.nix-ld.enable = true;
  services.vscode-server.enable = true;
  users.users.nixos.shell = pkgs.fish;
  wsl = {
    enable = true;
    defaultUser = "nixos";
    extraBin = with pkgs; [
      { src = "${coreutils}/bin/uname"; }
      { src = "${coreutils}/bin/dirname"; }
      { src = "${coreutils}/bin/readlink"; }
    ];
  };
}