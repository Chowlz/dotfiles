{ version, arch, home-manager, nix-darwin, nixoswsl, nixpkgs, vscode-server, ... }:

{
  ubuntu = home-manager.lib.homeManagerConfiguration {
    pkgs = import nixpkgs { system = arch.x86_64-linux; };
    modules = [
      ../common/home.nix
      ./hosts/ubuntu/home.nix
      {
        home = {
          # TODO
          username = "";
          # TODO
          homeDirectory = "";
          stateVersion = "24.05";
        };
        modules.git = {
          # TODO
          user.email = "";
          user.name = "Charles Cruz";
        };
      }
    ];
  };
}