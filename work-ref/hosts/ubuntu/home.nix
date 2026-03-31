{ pkgs, ... }:

let
  packages = import ../../../common/packages.nix pkgs;
in {
  home.packages = [
      pkgs.minio-client
    ] ++
    packages.base.common ++
    packages.base.gnu-utils ++
    packages.infra.ansible ++
    packages.infra.kubernetes;
}