{ config, ... }:

{
  home.file.".npmrc".text = ''
    prefix=${config.home.homeDirectory}/.local/share/npm
    cache=${config.home.homeDirectory}/.local/share/npm/cache
    init-module=${config.home.homeDirectory}/.local/share/npm/npm-init.js
  '';
}
