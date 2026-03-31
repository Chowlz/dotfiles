{ system, inputs }:

{
  ubuntu = inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs { system = system.arch.x86_64-linux; };
    modules = [
      ../common/home.nix
      ./hosts/ubuntu/home.nix
      {
        home = {
          # TODO
          username = "";
          # TODO
          homeDirectory = "";
          stateVersion = system.version.stable;
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