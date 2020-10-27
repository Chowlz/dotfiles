{ stdenv, callPackage, fetchFromGitHub, fetchpatch, cmake, perl, pkgconfig, zlib,
  darwin, libiconv, installShellFiles, just, pandoc }:
let
  rustPlatform = callPackage ./../rust/rust-platform.nix {};
in
(rustPlatform "2020-08-27").buildRustPackage rec {
  pname = "exa";
  version = "0.9.0";

  cargoSha256 = "1pq73hzmw1fa5ysw4117ybfjnz5q7w1l70d628labj665ad5hhjb";

  src = fetchFromGitHub {
    owner = "ogham";
    repo = "exa";
    rev = "f0c63b64ecfdc749b6c4b4d3e0c09866062c4771";
    sha256 = "1ph79840zqyiqsc5cy94am29f43himymydagzxk6ra499yy7xizf";
  };

  nativeBuildInputs = [ cmake pkgconfig perl just pandoc installShellFiles ];
  buildInputs = [ zlib ]
  ++ stdenv.lib.optionals stdenv.isDarwin [
    libiconv darwin.apple_sdk.frameworks.Security ]
  ;

  outputs = [ "out" "man" ];

  postPatch = ''
    substituteInPlace src/output/table.rs \
      --replace "/usr/share/zoneinfo" "/etc/zoneinfo"
  '';

  postInstall = ''
    just man
    installManPage target/man/exa.1
    installShellCompletion \
      --name exa completions/completions.bash \
      --name exa.fish completions/completions.fish \
      --name _exa completions/completions.zsh
  '';

  # Some tests fail, but Travis ensures a proper build
  doCheck = false;

  meta = with stdenv.lib; {
    description = "Replacement for 'ls' written in Rust";
    longDescription = ''
      exa is a modern replacement for ls. It uses colours for information by
      default, helping you distinguish between many types of files, such as
      whether you are the owner, or in the owning group. It also has extra
      features not present in the original ls, such as viewing the Git status
      for a directory, or recursing into directories with a tree view. exa is
      written in Rust, so it’s small, fast, and portable.
    '';
    homepage = "https://the.exa.website";
    license = licenses.mit;
    maintainers = with maintainers; [ ehegnes lilyball globin ];
  };
}
