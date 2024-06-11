{ lib, pkgs, inputs, ... }:

let
  packages = import ../../common/packages.nix pkgs;
in {
  environment.systemPackages =
    packages.aws ++
    packages.clojure ++
    packages.common ++
    packages.kubernetes ++
    packages.os ++
    packages.ssh;
  environment.shellAliases = {
    docker = "com.docker.cli.exe";
  };
  networking.hostName = "nixos-wsl";
  wsl = {
    docker-desktop.enable = true;
  };
}