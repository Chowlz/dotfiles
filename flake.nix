{
  description = "Charles's Nix Flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/2c3e5ec5df46d3aeee2a1da0bfedd74e21f4bf3a"; # nixos-25.11 (2026.01.13)
    # Determinate Systems Nixpkgs
    determinate-nixpkgs.url = "https://flakehub.com/f/nixos/nixpkgs/0.1"; # Unstable

    determinate = {
      url = "https://flakehub.com/f/DeterminateSystems/determinate/3";
      inputs.nixpkgs.follows = "determinate-nixpkgs";
    };
    ds-home-manager = {
      url = "https://flakehub.com/f/nix-community/home-manager/0.1"; # Unstable
      inputs.nixpkgs.follows = "determinate-nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixoswsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "https://flakehub.com/f/nix-darwin/nix-darwin/0.1"; # Unstable
      inputs.nixpkgs.follows = "determinate-nixpkgs";
    };
    vscode-server = {
      url = "github:nix-community/nixos-vscode-server";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, ... }@inputs:
  let
    system = {
      version = {
        stable = "25.11";
        unstable = "26.05";
      };
      arch = {
        aarch64-darwin = "aarch64-darwin";
        x86_64-linux = "x86_64-linux";
      };
    };
    git-config = {
      user.email = "mail@charlescruz.dev";
      user.name = "Charles Cruz";
    };
  in {
    darwinConfigurations = {
      darwin = inputs.nix-darwin.lib.darwinSystem {
        system = system.arch.aarch64-darwin;
        modules = [
          # Add the determinate nix-darwin module
          inputs.determinate.darwinModules.default
          # Base config
          ({ config, pkgs, lib, ... }: {
            users.users.charles = {
              name = "charles";
              home = "/Users/charles";
            };
          })
          # Home-manager
          inputs.ds-home-manager.darwinModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.charles = {
              imports = [ ./common/home.nix ];
              home = {
                username = "charles";
                sessionPath = [
                  "/opt/homebrew/bin"
                  "/opt/homebrew/sbin"
                ];
                stateVersion = system.version.unstable;
              };
              modules.git = git-config;
            };
          }
          # Darwin configuration
          ./hosts/darwin/configuration.nix
          ./hosts/darwin/determinate-nix.nix
        ];
      };
    };
    devShells.${system.arch.aarch64-darwin}.default =
      import ./hosts/darwin/dev-shells.nix { inherit system inputs; };
    nixosConfigurations = {
      nixos-wsl = inputs.nixpkgs.lib.nixosSystem {
        system = system.arch.x86_64-linux;
        modules = [
          ./common/host.nix
          ./common/nixos-wsl.nix
          ./hosts/nixos-wsl/configuration.nix
          ({ ... }: { system.stateVersion = system.version.stable; })
          inputs.home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.nixos = {
              imports = [ ./common/home.nix ];
              home = {
                username = "nixos";
                homeDirectory = "/home/nixos";
                stateVersion = system.version.stable;
              };
              modules.git = git-config // { wsl-ssh-1password = true; };
            };
          }
          inputs.nixoswsl.nixosModules.wsl
          inputs.vscode-server.nixosModules.default
        ];
      };
    } //
    (if builtins.pathExists ./work/nixos.nix
      then (import ./work/nixos.nix (system // inputs)) else { });
    homeConfigurations = { } //
    (if builtins.pathExists ./work/home-manager.nix
      then (import ./work/home-manager.nix { inherit system inputs; }) else { });
  };
}