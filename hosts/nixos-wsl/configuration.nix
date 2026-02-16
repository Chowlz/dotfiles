{ lib, pkgs, inputs, ... }:

let
  packages = import ../../common/packages.nix pkgs;
in {
  # Packages
  environment.systemPackages =
    packages.base.common ++
    packages.base.gnu-utils ++
    packages.infra.homelab ++
    packages.infra.kubernetes ++
    packages.languages.clojure ++
    packages.languages.go ++
    packages.languages.nodejs;
  # Docker
  environment.shellAliases = {
    docker = "com.docker.cli.exe";
  };
  # WSL
  networking.hostName = "nixos-wsl";
  wsl = {
    docker-desktop.enable = true;
  };
}