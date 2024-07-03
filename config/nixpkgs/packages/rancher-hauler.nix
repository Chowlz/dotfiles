{ lib, stdenv, fetchzip, ... }:

let
  pname = "rancher-hauler";
  version = "1.0.4";
  src = {
    aarch64-darwin = fetchzip {
      url = "https://github.com/rancherfederal/hauler/releases/download/v${version}/hauler_${version}_darwin_arm64.tar.gz";
      hash = "sha256-jhywr2jnab04AggFEx++GgqLH+3uxIz6FjBogfoYx9M=";
      stripRoot = false;
    };
    x86_64-darwin = fetchzip {
      url = "https://github.com/rancherfederal/hauler/releases/download/v${version}/hauler_${version}_darwin_amd64.tar.gz";
      hash = "sha256-8KgwSQO4QpM6dpV42bLrgN8a/bc/M2+yhr3HzFm5J0Y=";
      stripRoot = false;
    };
    aarch64-linux = fetchzip {
      url = "https://github.com/rancherfederal/hauler/releases/download/v${version}/hauler_${version}_linux_arm64.tar.gz";
      hash = "sha256-vgB0usmZF1ifgy/VJwXFGqarwpdLBBGInl8KUxEzMg0=";
      stripRoot = false;
    };
    x86_64-linux = fetchzip {
      url = "https://github.com/rancherfederal/hauler/releases/download/v${version}/hauler_${version}_linux_amd64.tar.gz";
      hash = "sha256-3V1Yn49MzA3tqFp4+iYc+LU1NLBS44pxwLNKyKgiqbQ=";
      stripRoot = false;
    };
  }.${stdenv.system} or (throw "${pname}-${version}: ${stdenv.system} is unsupported.");
in
stdenv.mkDerivation rec {
  inherit pname version src;
  installPhase = ''
    mkdir -p "$out/bin"
    mv hauler "$out/bin"
  '';
}