{ lib, pkgs, inputs, ... }:

{
  environment.shellAliases = {
    docker = " com.docker.cli.exe";
  };
  networking.hostName = "nixos-wsl";
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  programs.fish.enable = true;
  programs.nix-ld.enable = true;
  services.vscode-server.enable = true;
  users.users.nixos.shell = pkgs.fish;
  wsl = {
    enable = true;
    defaultUser = "nixos";
    docker-desktop.enable = true;
    extraBin = with pkgs; [
      { src = "${coreutils}/bin/uname"; }
      { src = "${coreutils}/bin/dirname"; }
      { src = "${coreutils}/bin/readlink"; }
    ];
    wslConf.network.hostname = "wsl";
  };
}