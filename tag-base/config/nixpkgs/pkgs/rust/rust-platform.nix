{ callPackage, fetchFromGitHub, makeRustPlatform }:

# The date of the nighly version to use.
date:

let
  # 09/29x/2020
  mozillaOverlay = fetchFromGitHub {
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    rev = "57c8084c7ef41366993909c20491e359bbb90f54";
    sha256 = "0lchhjys1jj8fdiisd2718sqd63ys7jrj6hq6iq9l1gxj3mz2w81";
  };
  mozilla = callPackage "${mozillaOverlay.out}/package-set.nix" {};
  rustNightly = (mozilla.rustChannelOf { inherit date; channel = "nightly"; }).rust;
in makeRustPlatform {
  cargo = rustNightly;
  rustc = rustNightly;
}
