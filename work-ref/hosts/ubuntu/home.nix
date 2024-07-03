{ pkgs, ... }:

let
  packages = import ../../../common/packages.nix pkgs;
in {
  home.packages =
    packages.common ++
    packages.os;
}