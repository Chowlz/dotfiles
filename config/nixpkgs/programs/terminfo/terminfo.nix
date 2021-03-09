{ lib, pkgs, ... }:

let
  inherit (pkgs) stdenv runCommand ncurses;

  tic = if stdenv.isDarwin
        then "/usr/bin/tic"
        else "${lib.getBin ncurses}/bin/tic";

  src = ''
    # Use colon separators.
    xterm-24bit|xterm with 24-bit direct color mode,
      use=xterm-256color,
      setb24=\E[48:2:%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm,
      setf24=\E[38:2:%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm,
    # Use semicolon separators.
    xterm-24bits|xterm with 24-bit direct color mode,
      use=xterm-256color,
      setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
      setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
  '';

  # Terminfo file for xterm and konsole with 24-bit colors.
  terminfo-xterm-24bit = runCommand "terminfo-xterm-24bit" {} ''
    cat >terminfo-xterm-24bit.src <<EOF
    ${src}
    EOF
    mkdir -p "$out/share/terminfo"
    ${tic} -x -o "$out/share/terminfo" terminfo-xterm-24bit.src
  '';
in {
  home.file.".terminfo".source = "${terminfo-xterm-24bit}/share/terminfo";
}
