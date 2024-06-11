{ lib, pkgs, inputs, ... }:

{
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
    wslConf.network.hostname = "wsl";
  };
}
