{ version, arch, home-manager, nix-darwin, nixoswsl, nixpkgs, vscode-server, ... }:

{
  laptop = nixpkgs.lib.nixosSystem {
    system = arch.x86_64-linux;
    modules = [
      ../common/host.nix
      ../common/nixos-wsl.nix
      ./hosts/laptop/configuration.nix
      ./hosts/laptop/pki.nix
      ({ ... }: { system.stateVersion = version; })
      home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.nixos = {
          imports = [ ../common/home.nix ];
          home = {
            username = "nixos";
            homeDirectory = "/home/nixos";
            stateVersion = version;
          };
          modules.git = {
            # TODO
            user.email = "";
            user.name = "Charles Cruz";
            wsl-ssh-1password = true;
          };
        };
      }
      nixoswsl.nixosModules.wsl
      vscode-server.nixosModules.default
    ];
  };
}