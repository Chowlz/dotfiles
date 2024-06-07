{
  description = "Charles's Nix Flake";

  inputs = {
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nixoswsl.inputs.nixpkgs.follows = "nixpkgs";
    nixoswsl.url = "github:nix-community/NixOS-WSL";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    vscode-server.url = "github:nix-community/nixos-vscode-server";
  };

  outputs = { self, home-manager, nix-darwin, nixoswsl, nixpkgs, vscode-server, ... }@inputs:
  let
    version = "24.05";
    systems = {
      aarch64-darwin = "aarch64-darwin";
      x86_64-linux = "x86_64-linux";
    };
  in {
    darwinConfigurations = {
      darwin = nix-darwin.lib.darwinSystem {
        inherit inputs;
        system = systems.aarch64-darwin;
        modules = [
          ./hosts/darwin/configuration.nix
        ];
      };
    };
    nixosConfigurations = {
      nixos-wsl = nixpkgs.lib.nixosSystem {
        system = systems.x86_64-linux;
        modules = [
          ./hosts/nixos-wsl/configuration.nix
          ./common/host.nix
          ./common/packages.nix
          ({ pkgs, ... }: { system.stateVersion = version; })
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.nixos = {
              imports = [ ./common/home.nix ];
              home = {
                username = "nixos";
                homeDirectory = "/home/nixos";
                stateVersion = version;
              };
              modules.git = {
                user.email = "mail@charlescruz.dev";
                user.name = "Charles Cruz";
                wsl-ssh-1password = true;
              };
            };
          }
          nixoswsl.nixosModules.wsl
          vscode-server.nixosModules.default
        ];
      };
    };
  };
}